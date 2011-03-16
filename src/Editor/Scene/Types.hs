{-# language NamedFieldPuns, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Editor.Scene.Types where


import Data.SelectTree
import qualified Data.Indexable as I
import Data.Indexable hiding (length, toList, findIndices, fromList)
import Data.Abelian
import Data.Initial

import Graphics.Qt

import Utils

import Base

import Object

import Sorts.Robots


-- type SceneMonad = StateT (EditorScene Sort_) IO


-- * getters

getSelectedLayerContent :: EditorScene Sort_ -> Indexable (EditorObject Sort_)
getSelectedLayerContent scene =
    ((scene ^. editorObjectsA) !|| selectedLayer scene) ^. contentA

-- | get the object that is actually selected by the cursor
getSelectedObject :: EditorScene Sort_ -> Maybe (EditorObject Sort_)
getSelectedObject scene =
    flip fmap (selected scene) $
        \ (layerIndex, i) -> (((scene ^. editorObjectsA) !|| layerIndex) ^. contentA) !!! i

-- | returns all Indices (to the mainLayer) for robots
getRobotIndices :: Sort sort o => EditorScene sort -> [Index]
getRobotIndices scene =
    I.findIndices (isRobot . editorSort) $ (mainLayer (scene ^. editorObjectsA) ^. contentA)

getCursorSize :: Sort sort o => EditorScene sort -> (Size Double)
getCursorSize s@EditorScene{} =
    size $ getSelected $ availableSorts s

-- | returns an object from the main layer
getMainlayerEditorObject :: EditorScene sort -> Index -> EditorObject sort
getMainlayerEditorObject scene i = os !!! i
  where
    os = mainLayerIndexable $ (scene ^. editorObjectsA)

-- returns the wanted cursor step
getCursorStep :: EditorScene Sort_ -> EditorPosition
getCursorStep s = case cursorStep s of
    Just x -> x
    Nothing ->
        let (Size x y) = size $ getSelected $ availableSorts s
        in EditorPosition x y


-- * Setters

setCursorStep :: EditorScene s -> Maybe EditorPosition -> EditorScene s
setCursorStep scene x = scene{cursorStep = x}

-- | adds a new default Layer to the EditorScene
addDefaultBackground :: EditorScene Sort_ -> EditorScene Sort_
addDefaultBackground s@EditorScene{editorObjects = (Grounds backgrounds mainLayer foregrounds)} =
    s{editorObjects = objects'}
  where
    objects' = Grounds (backgrounds >: initial) mainLayer foregrounds

-- | adds a new default Layer to the EditorScene
addDefaultForeground :: EditorScene Sort_ -> EditorScene Sort_
addDefaultForeground s@EditorScene{editorObjects = (Grounds backgrounds mainLayer foregrounds)} =
    s{editorObjects = objects'}
  where
    objects' = Grounds backgrounds mainLayer (initial <: foregrounds)

-- * modification

modifySorts :: (SelectTree Sort_ -> SelectTree Sort_) -> EditorScene Sort_ -> EditorScene Sort_
modifySorts f scene@EditorScene{availableSorts} = scene{availableSorts = f availableSorts}

-- | returns if an object is currently in the copy selection
inCopySelection :: Sort s x => EditorScene s -> EditorObject s -> Bool
inCopySelection EditorScene{editorMode = SelectionMode endPosition, cursor} object =
    (editorPosition object `pBetween` range) &&
    objectEndPosition `pBetween` range
  where
    Size w h = size $ editorSort object
    objectEndPosition = editorPosition object +~ EditorPosition w (- h)
    range = (cursor, endPosition)

    pBetween (EditorPosition x y) (EditorPosition x1 y1, EditorPosition x2 y2) =
        (x `between` (x1, x2)) &&
        (y `between` (y1, y2))
    between x (a, b) = x >= min a b && x <= max a b


cutSelection :: EditorScene Sort_ -> EditorScene Sort_
cutSelection scene =
    editorObjectsA .> layerA (selectedLayer scene) ^:
        (modifyContent deleteCutObjects) $
    scene{editorMode = NormalMode, clipBoard = clipBoard}
  where
    deleteCutObjects :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
    deleteCutObjects = foldr (.) id (map deleteByIndex cutIndices)
    clipBoard :: [EditorObject Sort_]
    clipBoard = map (moveSelectionToZero scene) $
        map (\ i -> getSelectedLayerContent scene !!! i) cutIndices
    cutIndices = findCopySelectionIndices scene

-- | deletes the selected objects without changing the clipboard contents.
-- implemented in terms of cutSelection (huzzah for non-destructive updates)
deleteSelection :: EditorScene Sort_ -> EditorScene Sort_
deleteSelection scene = (cutSelection scene){clipBoard = clipBoard scene}

copySelection :: EditorScene Sort_ -> EditorScene Sort_
copySelection scene =
    scene{editorMode = NormalMode, clipBoard = clipBoard}
  where
    clipBoard :: [EditorObject Sort_]
    clipBoard = map (moveSelectionToZero scene) $ 
        map (\ i -> getSelectedLayerContent scene !!! i) copyIndices
    copyIndices = findCopySelectionIndices scene

findCopySelectionIndices :: EditorScene Sort_ -> [Index]
findCopySelectionIndices scene =
    I.findIndices (inCopySelection scene) $ getSelectedLayerContent scene

moveSelectionToZero :: EditorScene Sort_ -> EditorObject Sort_ -> EditorObject Sort_
moveSelectionToZero scene@EditorScene{editorMode = SelectionMode (EditorPosition x2 y2)} =
    modifyEditorPosition (-~ EditorPosition x y) >>>
    modifyOEMEditorPositions (-~ EditorPosition x y)
  where
    x = min x1 x2
    y = max y1 y2
    EditorPosition x1 y1 = cursor scene

pasteClipboard :: EditorScene Sort_ -> EditorScene Sort_
pasteClipboard scene =
    editorObjectsA .> layerA (selectedLayer scene) ^: (modifyContent addClipboard) $
    scene
  where
    addClipboard :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
    addClipboard = 
        foldr (>>>) id $ map (\ o ix -> ix >: o) $
        map (modifyEditorPosition (+~ cursor scene)) $
        map (modifyOEMEditorPositions (+~ cursor scene)) $
        clipBoard scene
