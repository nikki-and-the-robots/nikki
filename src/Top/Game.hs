{-# language ScopedTypeVariables #-}

module Top.Game (playLevel) where


import Data.IORef

import Physics.Chipmunk

import Utils

import Base

import Game.Scene.Camera
import Game.MainLoop

import Sorts.Nikki (uniqueNikki)

import Top.Initialisation


playLevel :: Application -> AppState -> Bool -> EditorScene Sort_ -> AppState
playLevel app parent editorTestMode editorScene = NoGUIAppState $ do
    space <- io $ initSpace gravity
    scene :: Scene Object_ <- rm2m $
        initScene app (editorLevelFile editorScene) space
            (editorScene ^. cachedTiles)
            (uniqueNikki app (editorScene ^. editorObjects))
    let (NikkiMode nikkiIndex) = scene ^. mode
    cameraStateRef <- io $ newIORef $ initialCameraState nikkiIndex
    let retryLevel = this
    return $ gameAppState app parent editorTestMode
        (GameState space cameraStateRef scene retryLevel)
  where
    this = playLevel app parent editorTestMode editorScene
