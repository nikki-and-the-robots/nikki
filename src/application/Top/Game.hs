
module Top.Game (playLevel) where


import Data.IORef

import Control.Concurrent
import Control.Monad.State

import Graphics.Qt

import Utils

import Base.Grounds
import Base.GlobalCatcher
import Base.FPSState

import Object

import Editor.Scene

import Game.Scene
import Game.MainLoop

import Top.Application


playLevel :: Application -> GameState -> AppState -> AppState
playLevel app gameAppState parent = AppState $ do
    sceneMVar <- newMVar $ scene gameAppState
    fpsRef <- initialFPSRef
    setDrawingCallbackAppWidget (window app) (Just $ render fpsRef sceneMVar)

    setRenderLooped (window app) True
    runStateT (logicLoop app sceneMVar) gameAppState
    setRenderLooped (window app) False

    return parent

  where
    render fpsRef sceneMVar ptr = globalCatcher $ do

        tickFPSRef fpsRef

        scene <- readMVar sceneMVar
        Game.Scene.renderScene ptr scene



