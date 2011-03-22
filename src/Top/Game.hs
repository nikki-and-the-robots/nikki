{-# language ScopedTypeVariables #-}

module Top.Game (playLevel) where


import Data.IORef

import Control.Concurrent
import Control.Monad.State

import Physics.Chipmunk
import Graphics.Qt

import Utils

import Base

import Profiling.FPS

import Object

import Game.Scene
import Game.Scene.Camera
import Game.MainLoop

import Top.Initialisation


playLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
playLevel app parent editorScene = AppState $ withSpace gravity $ \ space -> do
    scene :: Scene Object_ <- io $ initScene space (editorScene ^. editorObjectsA)
    let (NikkiMode nikkiIndex) = mode scene
    sceneMVar <- io newEmptyMVar
    fpsRef <- initialFPSRef
    configuration <- getConfiguration
    io $ do
        nikkiPos <- getPosition $ getControlledChipmunk scene $
                        scene ^. mainLayerObjectA nikkiIndex
        cameraStateRef <- io $ newIORef initialCameraState
        setDrawingCallbackGLContext (window app)
            (Just $ render fpsRef cameraStateRef sceneMVar configuration)

        postGUI (window app) $ setRenderingLooped (window app) True
    void $ runStateT (gameLoop app sceneMVar) (GameState space scene)
    io $ postGUI (window app) $ setRenderingLooped (window app) False
    io $ setDrawingCallbackGLContext (window app) Nothing

    return parent

  where
    render fpsRef cameraStateRef sceneMVar configuration ptr = do

        (scene, debugging) <- readMVar sceneMVar
        runStateTFromIORef cameraStateRef $
            Game.Scene.renderScene app configuration ptr scene debugging

        tickFPSRef (alphaNumericFont $ applicationPixmaps app) ptr fpsRef
