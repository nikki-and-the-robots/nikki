{-# language FlexibleContexts, NamedFieldPuns #-}

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

import Data.Set (member)
import Data.SelectTree
import qualified Data.Indexable as I
import Data.Indexable (Index, (>:), deleteByIndex, indexA)
import Data.Abelian
import Data.Accessor
import Data.List (isPrefixOf)

import Control.Monad.State

import Graphics.Qt

import Utils

import Base

import Object

import Sorts.Nikki

import Editor.Pickle
import Editor.Scene.Types
import Editor.Scene.Rendering
import qualified Editor.Scene.RenderOrdering as RenderOrdering


-- | looks, if there is an object under the cursor (and therefore selected)
-- in the selected layer
searchSelectedObject :: EditorScene Sort_ -> Maybe (GroundsIndex, Index)
searchSelectedObject s@EditorScene{selectedLayer_} =
    let indices = I.findIndices isSelected $
            s ^. editorObjects ^. layerA selectedLayer_ ^. content
        isSelected o = lowerCorner o == cursor s
        lowerCorner o = o ^. editorPosition
    in case indices of
        [] -> Nothing
        ll -> Just $ (selectedLayer_, last ll)

-- * normalizers

updateSelected :: EditorScene Sort_ -> EditorScene Sort_
updateSelected s = s{selected = searchSelectedObject s}

-- * constructors

-- | the initial editor scene
initEditorScene :: SelectTree Sort_ -> LevelFile
    -> DiskLevel -> EditorScene Sort_
initEditorScene sorts editorLevelFile (DiskLevel objects cachedTiles) =
    updateSelected EditorScene{
        editorLevelFile,
        cursor = zero,
        cursorStep = Just $ EditorPosition 64 64,
        availableSorts_ = (if useNonPublicSorts then id else removeNonPublicSorts) sorts,
        editorObjects_ = objects,
        selectedLayer_ = MainLayer,
        selected = Nothing,
        editorMode = NormalMode,
        clipBoard = [],
        cachedTiles_ = cachedTiles
      }

-- | Used to provisionally remove all non-public (tutorial and story-mode)
-- objects from the editor.
-- (without rendering the respective levels unplayable)
removeNonPublicSorts :: SelectTree Sort_ -> SelectTree Sort_
removeNonPublicSorts = mkSortsSelectTree . filter isPublicSort . ftoList

isPublicSort :: Sort_ -> Bool
isPublicSort sort =
    not ("tutorial/" `isPrefixOf` id) &&
    not ("story-mode/" `isPrefixOf` id)
  where
    id = getSortId $ sortId sort

-- | sets the position of Nikki (more precisely all Nikkis in the EditorScene) to the given value.
setNikkiPosition :: EditorPosition -> EditorScene Sort_ -> EditorScene Sort_
setNikkiPosition position =
    editorObjects .> mainLayer ^: fmap modifyNikki
  where
    modifyNikki :: EditorObject Sort_ -> EditorObject Sort_
    modifyNikki o = if isNikki (editorSort o) then editorPosition ^= position $ o else o

-- * manipulating

-- | Updates the editor scene for a given key press.
-- Returns True in case the key was recognized and acted upon.
updateEditorScene :: (MonadState (EditorScene Sort_) m, MonadIO m) =>
    Configuration -> Application -> AppEvent -> m Bool
updateEditorScene config app (Press b@KeyboardButton{}) = do
    acted <- keyPress config app b
    when acted $ do
        modify updateSelected
        modify normalizeOEMStates
    return acted
updateEditorScene _ _ _ = return False


-- * gamepad buttons
-- Start (== Escape) is handled above in Editor.MainLoop

-- | returns if the keyboard event was handled
-- (either by updating the state or in case of an exception)
keyPress :: (MonadState (EditorScene Sort_) m, MonadIO m) =>
    Configuration -> Application -> Button -> m Bool
keyPress config app b = do
    scene <- get
    let newScene = case editorMode scene of
            NormalMode -> convert $ normalMode (key b) scene
            ObjectEditMode{} -> objectEditModeUpdate b scene
            SelectionMode{} -> convert $ selectionMode (key b) scene
    case newScene of
        (Left OEMNothing) -> return False
        (Left OEMError) -> do
            triggerSound config $ errorSound $ applicationSounds app
            return True
        (Right newScene) -> do
            -- acted on key event
            put newScene
            return True
  where
    convert :: Maybe a -> OEMUpdateMonad a
    convert (Just x) = return x
    convert Nothing = oemNothing

-- * Main Editor mode

-- arrow keys
normalMode :: Key -> EditorScene Sort_ -> Maybe (EditorScene Sort_)
normalMode LeftArrow scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in Just scene{cursor = (EditorPosition (x - sx) y)}
normalMode RightArrow scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in Just scene{cursor = (EditorPosition (x + sx) y)}
normalMode UpArrow scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in Just scene{cursor = (EditorPosition x (y - sy))}
normalMode DownArrow scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in Just scene{cursor = (EditorPosition x (y + sy))}

-- add object
normalMode key scene@EditorScene{cursor, selectedLayer_} | isEditorA key =
    Just $ editorObjects .> layerA selectedLayer_ ^: mod $
        scene
  where
    mod = modifyContent (RenderOrdering.sortMainLayer . (>: new))
    new = mkEditorObject selectedSort cursor
    selectedSort = getSelected $ scene ^. availableSorts

-- delete selected object
normalMode key scene@EditorScene{} | isEditorB key =
    case selected scene of
        Nothing -> Just scene
        (Just (layerIndex, i)) ->
            let mod = modifyContent (deleteByIndex i)
            in Just $ editorObjects .> layerA layerIndex ^: mod $
                scene

-- skip through available objects
normalMode D scene@EditorScene{} =
    Just $ availableSorts ^: selectNext $ scene
normalMode A scene@EditorScene{} =
    Just $ availableSorts ^: selectPrevious $ scene

-- cycle through objects under cursor
-- (ordering of rendering will be automated)
normalMode B scene@EditorScene{selected = Just (layerIndex, i)} =
    let mod = modifyContent (I.toHead i)
    in Just $ editorObjects .> layerA layerIndex ^: mod $ scene

-- change cursor step size

normalMode key scene | key `elem` [W, S] =
    Just $ changeCursorStepSize key scene

-- * Layers

normalMode Plus s =
    Just $
        selectedLayer ^: nextGroundsIndex (s ^. editorObjects) $
        s
normalMode Minus s =
    Just $
        selectedLayer ^: previousGroundsIndex (s ^. editorObjects) $
        s

-- * paste from clipBoard

normalMode V s = Just $ pasteClipboard s

normalMode Space s = Just $ toSelectionMode s

normalMode _ _scene = Nothing


-- * object edit mode

objectEditModeUpdate :: Button -> EditorScene Sort_ -> OEMUpdateMonad (EditorScene Sort_)
objectEditModeUpdate x scene@EditorScene{editorMode = ObjectEditMode i} = do
    let Just oldOemState = scene ^. acc
    case oemUpdate scene x oldOemState of
        (Left OEMNothing) -> oemNothing
        (Left OEMError) -> oemError
        (Right x) -> return $ acc ^= Just x $ scene
  where
    acc :: Accessor (EditorScene Sort_) (Maybe OEMState)
    acc = editorObjects .> mainLayer .> content .> indexA i .> editorOEMState


-- * selection mode

selectionMode :: Key -> EditorScene Sort_ -> Maybe (EditorScene Sort_)
selectionMode key scene@EditorScene{editorMode = SelectionMode pos}
    | key `member` allArrowKeys =
        Just scene{editorMode = SelectionMode (changeSelectionPosition key pos)}
  where
    changeSelectionPosition UpArrow (EditorPosition x y) = EditorPosition x (y - sy)
    changeSelectionPosition DownArrow (EditorPosition x y) = EditorPosition x (y + sy)
    changeSelectionPosition LeftArrow (EditorPosition x y) = EditorPosition (x - sx) y
    changeSelectionPosition RightArrow (EditorPosition x y) = EditorPosition (x + sx) y
    EditorPosition sx sy = getCursorStep scene
selectionMode X scene = Just $ cutSelection scene
selectionMode C scene = Just $ copySelection scene
selectionMode key scene | isEditorB key || Delete == key = Just $ deleteSelection scene
selectionMode key scene | key `elem` [W, S] = Just $ changeCursorStepSize key scene

selectionMode _ _scene = Nothing

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


-- * normalization of OEMStates
normalizeOEMStates :: Sort sort o => EditorScene sort -> EditorScene sort
normalizeOEMStates scene =
    editorObjects ^: fmap modEditorObject $
    scene
  where
    modEditorObject :: EditorObject sort -> EditorObject sort
    modEditorObject o =
        case o ^. editorOEMState of
            Nothing -> o
            Just oemState ->
                editorOEMState ^= (Just $ oemNormalize scene oemState) $ o
