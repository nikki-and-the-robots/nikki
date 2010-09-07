{-# language ScopedTypeVariables #-}

module Top.Game (playLevel) where


import Control.Concurrent
import Control.Monad.State

import Physics.Chipmunk
import Graphics.Qt

import Base.GlobalCatcher
import Base.FPSState
import Base.Types
import Base.Application

import Object

import Game.Scene
import Game.MainLoop

import Top.Initialisation


playLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
playLevel app parent editorScene = AppState $ do
    let scene :: (Space -> IO (Scene Object_)) = flip initScene (editorObjects editorScene)
    gameAppState <- initialState (application app) (window app) scene
    sceneMVar <- newEmptyMVar
    fpsRef <- initialFPSRef
    setDrawingCallbackAppWidget (window app) (Just $ render fpsRef sceneMVar)

    setRenderLooped (window app) True
    runStateT (gameLoop app sceneMVar) gameAppState
    setRenderLooped (window app) False

    return parent

  where
    render fpsRef sceneMVar ptr = globalCatcher $ do

        tickFPSRef fpsRef

        scene <- readMVar sceneMVar
        Game.Scene.renderScene ptr scene



