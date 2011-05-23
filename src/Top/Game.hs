{-# language ScopedTypeVariables #-}

module Top.Game (playLevel) where


import Data.IORef

import Physics.Chipmunk

import Utils

import Base

import Game.Scene.Camera
import Game.MainLoop

import Top.Initialisation


playLevel :: Application -> AppState -> Bool -> EditorScene Sort_ -> AppState
playLevel app parent editorTestMode editorScene = NoGUIAppState $ withSpace gravity $ \ space -> do
    scene :: Scene Object_ <- io $
        initScene app (editorLevelFile editorScene) space (editorScene ^. editorObjects)
    let (NikkiMode nikkiIndex) = scene ^. mode
    cameraStateRef <- io $ newIORef $ initialCameraState nikkiIndex
    runAppState app $ gameAppState app editorTestMode (GameState space cameraStateRef scene)
    return parent

