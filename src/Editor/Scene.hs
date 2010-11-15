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
    setNikkiPosition,
    updateSelected,
    updateEditorScene,
    renderEditorScene,
  ) where

import Data.Map hiding (map, filter, mapMaybe, size, member)
import Data.Set (member)
import Data.SelectTree
import qualified Data.Indexable as I
import Data.Indexable (Index, (>:), modifyByIndex, deleteByIndex)
import Data.Abelian

import Control.Monad.State

import Graphics.Qt

import Base.Grounds
import Base.Types
import Base.Events

import Object

import Editor.Scene.Types
import Editor.Scene.Rendering
import qualified Editor.Scene.RenderOrdering as RenderOrdering


-- | looks, if there is an object under the cursor (and therefore selected)
-- in the selected layer
searchSelectedObject :: EditorScene Sort_ -> Maybe (GroundsIndex, Index)
searchSelectedObject s@EditorScene{selectedLayer} =
    let indices = I.findIndices isSelected (content (editorObjects s !|| selectedLayer))
        isSelected o = lowerCorner o == cursor s
        lowerCorner o = editorPosition o
    in case indices of
        [] -> Nothing
        ll -> Just $ (selectedLayer, last ll)

-- * normalizers

updateSelected :: EditorScene Sort_ -> EditorScene Sort_
updateSelected s = s{selected = searchSelectedObject s}

-- * constructors

-- | the initial editor scene
initEditorScene :: SelectTree Sort_ -> Maybe String -> Grounds PickleObject -> IO (EditorScene Sort_)
initEditorScene sorts mPath pickledObjects = flip evalStateT empty $ do
    let objects :: Grounds (EditorObject Sort_) = fmap (pickleObject2EditorObject $ leafs sorts) pickledObjects
    pixmap <- get
    return $ updateSelected EditorScene{
        levelPath = mPath,
        cursor = zero,
        cursorStep = Just $ EditorPosition 64 64,
        availableSorts = sorts,
        editorObjects = objects,
        selectedLayer = MainLayer,
        selected = Nothing,
        editorMode = NormalMode,
        clipBoard = []
      }

-- | sets the position of Nikki (more precisely all Nikkis in the EditorScene) to the given value.
setNikkiPosition :: EditorPosition -> EditorScene Sort_ -> EditorScene Sort_
setNikkiPosition position =
    modifyEditorObjects (modifyMainLayer (fmap modifyNikki))
  where
    modifyNikki :: EditorObject Sort_ -> EditorObject Sort_
    modifyNikki o = if isNikki (editorSort o) then o{editorPosition = position} else o

-- * manipulating

updateEditorScene :: AppEvent -> EditorScene Sort_ -> EditorScene Sort_
updateEditorScene (Press button) scene =
    updateSelected $
    keyPress button scene
updateEditorScene (Release button) s = s


-- * gamepad buttons
-- Start (== Escape) is handled above in Editor.MainLoop

keyPress :: AppButton -> EditorScene Sort_ -> EditorScene Sort_
keyPress button scene =
    case editorMode scene of
        NormalMode -> normalMode button scene
        ObjectEditMode{} -> objectEditMode button scene
        SelectionMode{} -> selectionMode button scene

-- * Main Editor mode

-- arrow keys
normalMode LeftButton scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition (x - sx) y)}
normalMode RightButton scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition (x + sx) y)}
normalMode UpButton scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition x (y - sy))}
normalMode DownButton scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition x (y + sy))}

-- add object
normalMode AButton scene@EditorScene{cursor, selectedLayer} =
    scene{editorObjects = objects'}
  where
    objects' = modifySelectedLayer selectedLayer
        (modifyContent (RenderOrdering.sortMainLayer . (>: new)))
        (editorObjects scene)
    new = mkEditorObject selectedSort cursor
    selectedSort = getSelected $ availableSorts scene

-- delete selected object
normalMode BButton scene@EditorScene{} =
    case selected scene of
        Nothing -> scene
        (Just (layerIndex, i)) ->
            let newObjects = modifySelectedLayer layerIndex (modifyContent (deleteByIndex i)) (editorObjects scene)
            in scene{editorObjects = newObjects}

normalMode (KeyboardButton x _) scene = normalModeKeyboard x scene

normalMode _ s = s

-- * buttons pressed on the keyboard

normalModeKeyboard :: Key -> EditorScene Sort_ -> EditorScene Sort_

-- skip through available objects
normalModeKeyboard D scene@EditorScene{} =
    modifySorts selectNext scene
normalModeKeyboard A scene@EditorScene{} =
    modifySorts selectPrevious scene

-- cycle through objects under cursor
-- (ordering of rendering will be automated)
normalModeKeyboard B scene@EditorScene{editorObjects, selected = Just (layerIndex, i)} =
    let editorObjects' = modifySelectedLayer layerIndex (modifyContent (I.toHead i)) editorObjects
    in scene{editorObjects = editorObjects'}

-- change cursor step size

normalModeKeyboard key scene | key `elem` [W, S] =
    changeCursorStepSize key scene

-- * Layers

normalModeKeyboard Plus s@EditorScene{editorObjects, selectedLayer} =
    s{selectedLayer = modifyGroundsIndex editorObjects (+ 1) selectedLayer}
normalModeKeyboard Minus s@EditorScene{editorObjects, selectedLayer} =
    s{selectedLayer = modifyGroundsIndex editorObjects (subtract 1) selectedLayer}

-- * paste from clipBoard

normalModeKeyboard V s = pasteClipboard s

normalModeKeyboard Space s = toSelectionMode s

normalModeKeyboard _ scene = scene


-- * object edit mode

objectEditMode x s@EditorScene{editorMode = ObjectEditMode i} =
    s{editorObjects = objects'}
  where
    objects' = modifyMainLayer (modifyByIndex (modifyOEMState mod) i) $ editorObjects s
    mod :: OEMState Sort_ -> OEMState Sort_
    mod = updateOEM s x


-- * selection mode

selectionMode :: AppButton -> EditorScene Sort_ -> EditorScene Sort_
selectionMode button scene@EditorScene{editorMode = SelectionMode pos}
    | button `member` allArrowButtons =
        scene{editorMode = SelectionMode (changeSelectionPosition button pos)}
  where
    changeSelectionPosition UpButton (EditorPosition x y) = EditorPosition x (y - sy)
    changeSelectionPosition DownButton (EditorPosition x y) = EditorPosition x (y + sy)
    changeSelectionPosition LeftButton (EditorPosition x y) = EditorPosition (x - sx) y
    changeSelectionPosition RightButton (EditorPosition x y) = EditorPosition (x + sx) y
    EditorPosition sx sy = getCursorStep scene
selectionMode (KeyboardButton X _) scene = cutSelection scene
selectionMode (KeyboardButton C _) scene = copySelection scene
selectionMode BButton scene = deleteSelection scene
selectionMode (KeyboardButton key _) scene | key `elem` [W, S] = changeCursorStepSize key scene

selectionMode _ scene = scene

-- | changes the cursor's step size with W and S
changeCursorStepSize :: Key -> EditorScene Sort_ -> EditorScene Sort_
changeCursorStepSize W scene =
    case cursorStep scene of
        Nothing -> setCursorStep scene $ Just $ EditorPosition 1 1
        Just (EditorPosition x y) -> setCursorStep scene $ Just $ EditorPosition (x * 2) (y * 2)
changeCursorStepSize S scene =
    case cursorStep scene of
        Nothing -> setCursorStep scene Nothing
        Just (EditorPosition 1 1) -> setCursorStep scene Nothing
        Just (EditorPosition x y) -> setCursorStep scene $ Just $ EditorPosition (x / 2) (y / 2)
