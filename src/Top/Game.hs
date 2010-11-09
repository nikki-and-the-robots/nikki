{-# language ScopedTypeVariables #-}

module Top.Game (playLevel) where


import Data.IORef

import Control.Concurrent
import Control.Monad.State

import Physics.Chipmunk
import Graphics.Qt

import Utils

import Base.Constants
import Base.GlobalCatcher
import Base.FPSState
import Base.Types
import Base.Application

import Object

import Game.Scene
import Game.Scene.Camera
import Game.MainLoop

import Top.Initialisation


playLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
playLevel app parent editorScene = AppState $ withSpace gravity $ \ space -> do
    scene :: Scene Object_ <- initScene space (editorObjects editorScene)
    sceneMVar <- newEmptyMVar
    fpsRef <- initialFPSRef
    nikkiPos <- getPosition $ getControlledChipmunk $ getControlled scene
    cameraStateRef <- newIORef $ initialCameraState nikkiPos
    setDrawingCallbackAppWidget (window app)
        (Just $ render fpsRef cameraStateRef sceneMVar)

    setRenderingLooped (window app) True
    runStateT (gameLoop app sceneMVar) (GameState space scene)
    setRenderingLooped (window app) False
    setDrawingCallbackAppWidget (window app) Nothing

    return parent

  where
    render fpsRef cameraStateRef sceneMVar ptr = globalCatcher $ do

        tickFPSRef fpsRef

        (scene, debugging) <- readMVar sceneMVar
        runStateTFromIORef cameraStateRef $
            Game.Scene.renderScene ptr scene debugging
