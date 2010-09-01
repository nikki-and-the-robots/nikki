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
        cursorStep = const (EditorPosition 64 64),
        availableSorts = sorts,
        editorObjects = objects,
        selectedLayer = MainLayer,
        selected = Nothing,
        objectEditModeIndex = Nothing,
        debugMsgs = []
      }


-- * manipulating

updateEditorScene :: AppEvent -> EditorScene Sort_ -> EditorScene Sort_
updateEditorScene (Press button) scene =
    updateSelected $
    keyPress button scene
updateEditorScene (Release button) s = s



keyPress :: AppButton -> EditorScene Sort_ -> EditorScene Sort_

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

-- * buttons pressed on the keyboard

-- * object edit mode
-- keyPress Escape s@EditorScene{objectEditModeIndex = Just i} =
--     s{objectEditModeIndex = Nothing}
-- keyPress x s@EditorScene{objectEditModeIndex = Just i} =
--     s{editorObjects = objects'}
--   where
--     objects' = modifyMainLayer (modifyByIndex (modifyOEMState mod) i) $ editorObjects s
--     mod :: OEMState Sort_ -> OEMState Sort_
--     mod = updateOEM s x
-- 
-- keyPress Enter s@EditorScene{} =
--     case selected s of
--         Nothing -> s
--         Just i -> case objectEditModeMethods $ editorSort $ getMainObject s i of
--             Nothing -> s
--             Just _ -> s{objectEditModeIndex = Just i, editorObjects = objects'}
--               where
--                 objects' = modifyMainLayer (modifyByIndex (modifyOEMState mod) i) $ editorObjects s
--                 mod :: OEMState Sort_ -> OEMState Sort_
--                 mod = enterModeOEM s
-- -- skip through available objects
-- keyPress X scene@EditorScene{} =
--     modifySorts selectNext scene
-- keyPress Y scene@EditorScene{} =
--     modifySorts selectPrevious scene
-- 
-- -- put selected object to the back
-- keyPress B scene@EditorScene{editorObjects, selected = Just i} =
--     let mainLayer' = I.toHead i (mainLayerIndexable editorObjects)
--     in scene{editorObjects = editorObjects{mainLayer = mkMainLayer mainLayer'}}
-- -- put selected object to the front
-- keyPress F scene@EditorScene{editorObjects, selected = Just i} =
--     let mainLayer' = I.toLast i (mainLayerIndexable editorObjects)
--     in scene{editorObjects = editorObjects{mainLayer = mkMainLayer mainLayer'}}
-- 
-- -- * Layers
-- 
-- keyPress Plus s@EditorScene{editorObjects, selectedLayer} =
--     s{selectedLayer = modifyGroundsIndex editorObjects (+ 1) selectedLayer}
-- keyPress Minus s@EditorScene{editorObjects, selectedLayer} =
--     s{selectedLayer = modifyGroundsIndex editorObjects (subtract 1) selectedLayer}
-- 
-- 
-- -- * Menus
-- 
-- keyPress Escape s@EditorScene{debugMsgs} =
--     MenuScene s (topLevelMenu s) debugMsgs
-- 
-- -- selecting
-- keyPress DownArrow s@MenuScene{menu} = s{menu = selectFun (+ 1) menu}
-- keyPress UpArrow s@MenuScene{menu} = s{menu = selectFun (subtract 1) menu}
-- keyPress Shift s@MenuScene{mainScene, menu} =
--     case enterMenu menu mainScene of
--         Left menu' -> s{menu = menu'}
--         Right (Left errorMsg) -> addDebugMsg errorMsg mainScene
--         Right (Right scene') -> scene'
-- keyPress Escape s@MenuScene{menu} =
--     case exitMenu menu of
--         Nothing -> mainScene s
--         Just x -> s{menu = x}
-- 
-- 
-- -- * changing of cursorStep
-- 
-- keyPress k s@EditorScene{} | k `member` cursorStepShortCuts =
--     s{cursorStep = (cursorStepShortCuts ! k)}

keyPress _ s = s


-- | contains the shortcuts to change the vector the cursor is moved by the arrow keys.
cursorStepShortCuts :: Map Key (EditorScene Sort_ -> EditorPosition)
cursorStepShortCuts = fromList (
    (K0, fromSelectedPixmap) -- like the selected object
    : map (second (\ x -> const $ EditorPosition x x)) constSquareShortcuts)
  where
    fromSelectedPixmap :: EditorScene Sort_ -> EditorPosition
    fromSelectedPixmap EditorScene{availableSorts} =
        let (Size x y) = size $ getSelected availableSorts
        in EditorPosition x y
    -- | shortcuts that put cursorStep to a constant square
    constSquareShortcuts :: [(Key, Double)]
    constSquareShortcuts = [
        (K1, 1),
        (K2, fromUber 1),
        (K3, fromUber 4),
        (K4, fromUber 8),  -- 32
        (K5, fromUber 16), -- 64
        (K6, fromUber 32), -- 128
        (K7, fromUber 64),
        (K8, fromUber 128),
        (K9, fromUber 256)
      ]



-- topLevelMenu :: EditorScene Sort_ -> Menu (MenuLabel Sort_) (EditorScene Sort_)
-- topLevelMenu s = mkMenu (mkLabel "Menu") [
--     tileSelection s,
--     layerMenu s,
--     quit
--   ]






