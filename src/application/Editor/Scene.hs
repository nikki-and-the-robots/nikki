{-# language PatternGuards, NamedFieldPuns, DeriveDataTypeable, ScopedTypeVariables #-}

-- | Here the scene used in the level editor is implemented.
-- The scene contains all available objects and all set objects.
-- The scene implements different modi (via Sum-Types) to
-- do things like associations from terminals to robots.
-- The core function (at the end of the file) is 'keyPress',
-- that gets the user input events and modifies the scene.

module Editor.Scene (
    EditorScene(..),
    initEditorScene,
    updateEditorScene,
    renderEditorScene,
  ) where

import Data.Maybe
import Data.Map hiding (map, filter, mapMaybe, size)
import Data.SelectTree
import qualified Data.Indexable as I
import Data.Indexable (Index, (>:), modifyByIndex, deleteByIndex)
import Data.Menu hiding (selected)
import Data.Abelian

import Control.Monad.State
import Control.Arrow

import Graphics.Qt

import Utils

import Base.Constants
import Base.Grounds
import Base.Types
import Base.Events

import Object

import Editor.Scene.Types
import Editor.Scene.Rendering


-- | looks, if there is an object under the cursor (and therefore selected)
-- in the selected layer
searchSelectedObject :: EditorScene Sort_ -> Maybe Index
searchSelectedObject s@EditorScene{selectedLayer} =
    let indices = I.findIndices isSelected (content (editorObjects s !|| selectedLayer))
        isSelected o = lowerCorner o == cursor s
        lowerCorner o = editorPosition o
    in case indices of
        [] -> Nothing
        ll -> Just $ last ll

-- * normalizers

updateSelected :: EditorScene Sort_ -> EditorScene Sort_
updateSelected s = s{selected = searchSelectedObject s}

-- * constructors

-- | the initial editor scene
initEditorScene :: SelectTree Sort_ -> Maybe (String, Grounds PickleObject) -> IO (EditorScene Sort_)
initEditorScene sorts mObjects = flip evalStateT empty $ do
    let (path, objects :: Grounds (EditorObject Sort_)) = case mObjects of
                Nothing -> (Nothing, emptyGrounds)
                Just (p, os) ->
                    let objects = fmap (pickleObject2EditorObject $ leafs sorts) os
                    in (Just p, objects)
    pixmap <- get
    return $ updateSelected EditorScene{
        levelPath = path,
        cursor = zero,
        cursorStep = Just $ EditorPosition 64 64,
        availableSorts = sorts,
        editorObjects = objects,
        selectedLayer = MainLayer,
        selected = Nothing,
        editorMode = NormalMode,
        debugMsgs = []
      }


-- * manipulating

updateEditorScene :: AppEvent -> EditorScene Sort_ -> EditorScene Sort_
updateEditorScene (Press button) scene =
    updateSelected $
    keyPress button scene
updateEditorScene (Release button) s = s



keyPress :: AppButton -> EditorScene Sort_ -> EditorScene Sort_

-- * object edit mode

keyPress x s@EditorScene{editorMode = ObjectEditMode i} =
    s{editorObjects = objects'}
  where
    objects' = modifyMainLayer (modifyByIndex (modifyOEMState mod) i) $ editorObjects s
    mod :: OEMState Sort_ -> OEMState Sort_
    mod = updateOEM s x


-- * Main Editor mode

-- * gamepad buttons
-- Start (== Escape) is handled above in Editor.MainLoop

-- arrow keys
keyPress LeftButton scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition (x - sx) y)}
keyPress RightButton scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition (x + sx) y)}
keyPress UpButton scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition x (y - sy))}
keyPress DownButton scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition x (y + sy))}

-- add object
keyPress AButton scene@EditorScene{cursor, selectedLayer} =
    scene{editorObjects = objects'}
  where
    objects' = modifySelectedLayer selectedLayer (modifyContent (>: new)) (editorObjects scene)
    new = mkEditorObject selectedSort cursor
    selectedSort = getSelected $ availableSorts scene

-- delete selected object
keyPress BButton scene@EditorScene{selectedLayer} =
    case selected scene of
        Nothing -> scene
        (Just i) ->
            let newObjects = modifySelectedLayer selectedLayer (modifyContent (deleteByIndex i)) (editorObjects scene)
            in scene{editorObjects = newObjects}

keyPress (KeyboardButton x) scene = keyboardPress x scene

keyPress _ s = s

-- * buttons pressed on the keyboard

-- skip through available objects
keyboardPress D scene@EditorScene{} =
    modifySorts selectNext scene
keyboardPress A scene@EditorScene{} =
    modifySorts selectPrevious scene

-- cycle through objects under cursor
-- (ordering of rendering will be automated)
keyboardPress C scene@EditorScene{editorObjects, selected = Just i} =
    let mainLayer' = I.toHead i (mainLayerIndexable editorObjects)
    in scene{editorObjects = editorObjects{mainLayer = mkMainLayer mainLayer'}}

-- change cursor step size

keyboardPress W scene =
    case cursorStep scene of
        Nothing -> setCursorStep scene $ Just $ EditorPosition 1 1
        Just (EditorPosition x y) -> setCursorStep scene $ Just $ EditorPosition (x * 2) (y * 2)
keyboardPress S scene =
    case cursorStep scene of
        Nothing -> setCursorStep scene Nothing
        Just (EditorPosition 1 1) -> setCursorStep scene Nothing
        Just (EditorPosition x y) -> setCursorStep scene $ Just $ EditorPosition (x / 2) (y / 2)

-- * Layers

keyboardPress Plus s@EditorScene{editorObjects, selectedLayer} =
    s{selectedLayer = modifyGroundsIndex editorObjects (+ 1) selectedLayer}
keyboardPress Minus s@EditorScene{editorObjects, selectedLayer} =
    s{selectedLayer = modifyGroundsIndex editorObjects (subtract 1) selectedLayer}

keyboardPress _ scene = scene
