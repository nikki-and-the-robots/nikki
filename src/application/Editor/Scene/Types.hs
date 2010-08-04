{-# language NamedFieldPuns, FlexibleInstances, DeriveDataTypeable #-}

module Editor.Scene.Types where


import Data.SelectTree
import qualified Data.Indexable as I
import Data.Indexable hiding (length, toList, findIndices, fromList, empty)

import Control.Monad.State

import Graphics.Qt

import Base.Grounds
import Base.Types

import Object


data ControlData = ControlData [QtEvent] [Key]
  deriving Show

type SceneMonad = StateT (EditorScene Sort_) IO


-- * getters

-- | get the object that is actually selected by the cursor
getSelectedObject :: EditorScene Sort_ -> Maybe (EditorObject Sort_)
getSelectedObject EditorScene{selected = Just i, editorObjects, selectedLayer} =
    Just (content (editorObjects !|| selectedLayer) !!! i)
getSelectedObject EditorScene{selected = Nothing} = Nothing

-- | returns all Indices (to the mainLayer) for robots
getRobotIndices :: EditorScene Sort_ -> [Index]
getRobotIndices EditorScene{editorObjects} =
    I.findIndices (isRobot . editorSort) $ content $ mainLayer editorObjects

getCursorSize :: EditorScene Sort_ -> (Size Double)
getCursorSize s@EditorScene{} =
    size $ getSelected $ availableSorts s

-- | returns an object from the main layer
getMainObject :: EditorScene Sort_ -> Index -> EditorObject Sort_
getMainObject scene i = os !!! i
  where
    os = mainLayerIndexable $ editorObjects scene

-- getTerminalMRobot :: EditorScene -> Maybe EditorObject
-- getTerminalMRobot scene@TerminalScene{} =
--     case tmAvailableRobots scene of
--         (i : _) -> Just (getObject scene i)
--         [] -> Nothing


-- * Setters


addDebugMsg :: String -> EditorScene Sort_ -> EditorScene Sort_
addDebugMsg msg s =
    let msgs = debugMsgs s
    in s{debugMsgs = msg : msgs}

-- | adds a new default Layer to the EditorScene
addDefaultBackground :: EditorScene Sort_ -> EditorScene Sort_
addDefaultBackground s@EditorScene{editorObjects = (Grounds backgrounds mainLayer foregrounds)} =
    s{editorObjects = objects'}
  where
    objects' = Grounds (backgrounds >: initialLayer) mainLayer foregrounds

-- | adds a new default Layer to the EditorScene
addDefaultForeground :: EditorScene Sort_ -> EditorScene Sort_
addDefaultForeground s@EditorScene{editorObjects = (Grounds backgrounds mainLayer foregrounds)} =
    s{editorObjects = objects'}
  where
    objects' = Grounds backgrounds mainLayer (initialLayer <: foregrounds)

-- * modification

modifyEditorObjects :: (Grounds (EditorObject Sort_) -> Grounds (EditorObject Sort_)) -> EditorScene Sort_ -> EditorScene Sort_
modifyEditorObjects f s@EditorScene{editorObjects} = s{editorObjects = f editorObjects}

modifySorts :: (SelectTree Sort_ -> SelectTree Sort_) -> EditorScene Sort_ -> EditorScene Sort_
modifySorts f scene@EditorScene{availableSorts} = scene{availableSorts = f availableSorts}

