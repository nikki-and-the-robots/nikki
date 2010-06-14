{-# language PatternGuards, NamedFieldPuns, DeriveDataTypeable, ScopedTypeVariables #-}

-- | Here the scene used in the level editor is implemented.
-- The scene contains all available objects and all set objects.
-- The scene implements different modi (via Sum-Types) to
-- do things like associations from terminals to robots.
-- The core function (at the end of the file) is 'keyPress',
-- that gets the user input events and modifies the scene.

module Editor.Scene (
    EditorScene(..),
    getLevelName,
    ControlData(..),
    initScene,
    updateScene,
    renderScene,
  ) where

import Data.Maybe
import Data.Map hiding (map, filter, mapMaybe, size)
import Data.SelectTree
import qualified Data.Indexable as I
import Data.Indexable (Index, (>:), modifyByIndex)
import Data.Menu hiding (selected)
import Data.Abelian
import Data.Dynamic

import Control.Monad.State

import Graphics.Qt

import Utils

import Base.Constants
import Base.Grounds

import Object.Types

import Editor.Scene.Types
import Editor.Scene.Menu as Menu
import Editor.Scene.Rendering


-- | looks, if there is an object under the cursor (and therefore selected)
-- in the selected layer
searchSelectedObject :: EditorScene -> Maybe Index
searchSelectedObject s@EditorScene{selectedLayer} =
    let indices = I.findIndices isSelected (content (objects s !|| selectedLayer))
        isSelected o = lowerCorner o == cursor s
        lowerCorner o = editorPosition o
    in case indices of
        [] -> Nothing
        ll -> Just $ last ll

-- * normalizers

normSelected :: EditorScene -> EditorScene
normSelected s@EditorScene{} = s{selected = searchSelectedObject s}
normSelected x = x

-- * constructors

-- | the initial editor scene
initScene :: [IO [Sort_]] -> Maybe (String, Grounds PickleObject) -> IO EditorScene
initScene sortLoaders mObjects = flip evalStateT empty $ do

    sorts <- liftIO $ getAllSorts sortLoaders

    let (name, objects :: Grounds EditorObject) = case mObjects of
                Nothing -> (Nothing, emptyGrounds)
                Just (n, os) ->
                    let objects = fmap (pickleObject2EditorObject $ leafs sorts) os
                    in (Just n, objects)
    pixmap <- get
    return $ normSelected EditorScene{
        levelName = name,
        cursor = zero,
        cursorStep = const (EditorPosition 64 64),
        sorts = sorts,
        objects = objects,
        selectedLayer = MainLayer,
        selected = Nothing,
        objectEditModeIndex = Nothing,
        debugMsgs = []
      }

getAllSorts :: [IO [Sort_]] -> IO (SelectTree Sort_)
getAllSorts sortLoaders = do
    sorts <- concat <$> mapM id sortLoaders
    return $ Node "editor-Tiles" (I.fromList $ map Leaf sorts) 0



-- * manipulating

updateScene :: ControlData -> EditorScene -> EditorScene
updateScene (ControlData events held) scene =
    normSelected $ chainApp keyPress pressed scene
  where
    pressed = mapMaybe unwrapPress events
    unwrapPress (KeyPress x) = Just x
    unwrapPress _ = Nothing




keyPress :: Key -> EditorScene -> EditorScene

-- * Main Editor mode

-- * object edit mode
keyPress Escape s@EditorScene{objectEditModeIndex = Just i} =
    s{objectEditModeIndex = Nothing}
keyPress x s@EditorScene{objectEditModeIndex = Just i} =
    s{objects = objects'}
  where
    objects' = modifyMainLayer (modifyByIndex (modifyOEMState mod) i) $ objects s
    mod :: OEMState -> OEMState
    mod = updateOEM (toDyn s) x

keyPress Enter s@EditorScene{} =
    case selected s of
        Nothing -> s
        Just i -> case mkOEMState $ editorSort $ getMainObject s i of
            Nothing -> s
            Just _ -> s{objectEditModeIndex = Just i, objects = objects'}
              where
                objects' = modifyMainLayer (modifyByIndex (modifyOEMState mod) i) $ objects s
                mod :: OEMState -> OEMState
                mod = enterModeOEM (toDyn s)

-- arrow keys
keyPress LeftArrow scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition (x - sx) y)}
keyPress RightArrow scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition (x + sx) y)}
keyPress UpArrow scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition x (y - sy))}
keyPress DownArrow scene@EditorScene{cursor = (EditorPosition x y)} =
    let (EditorPosition sx sy) = getCursorStep scene
    in scene{cursor = (EditorPosition x (y + sy))}

-- add object
keyPress Space scene@EditorScene{cursor, selectedLayer} =
    scene{objects = objects'}
  where
    objects' = modifySelectedLayer selectedLayer (modifyContent (>: new)) (objects scene)
    new = mkEditorObject selectedSort cursor
    selectedSort = getSelected $ sorts scene

-- delete selected object
keyPress Delete scene@EditorScene{selectedLayer} =
    case selected scene of
        Nothing -> scene
        (Just i) ->
            let newObjects = modifySelectedLayer selectedLayer (modifyContent (flip I.deleteByIndex i)) (objects scene)
            in scene{objects = newObjects}

-- skip through available objects
keyPress X scene@EditorScene{} =
    modifySorts selectNext scene
keyPress Y scene@EditorScene{} =
    modifySorts selectPrevious scene

-- put selected object to the back
keyPress B scene@EditorScene{objects, selected = Just i} =
    let mainLayer' = I.toHead i (mainLayerIndexable objects)
    in scene{objects = objects{mainLayer = mkMainLayer mainLayer'}}


-- * Layers

keyPress Plus s@EditorScene{objects, selectedLayer} =
    s{selectedLayer = modifyGroundsIndex objects (+ 1) selectedLayer}
keyPress Minus s@EditorScene{objects, selectedLayer} =
    s{selectedLayer = modifyGroundsIndex objects (subtract 1) selectedLayer}


-- * Menus

keyPress Escape s@EditorScene{debugMsgs} =
    MenuScene s (topLevelMenu s) debugMsgs

-- selecting
keyPress DownArrow s@MenuScene{menu} = s{menu = selectFun (+ 1) menu}
keyPress UpArrow s@MenuScene{menu} = s{menu = selectFun (subtract 1) menu}
keyPress Shift s@MenuScene{mainScene, menu} =
    case enterMenu menu mainScene of
        Left menu' -> s{menu = menu'}
        Right (Left errorMsg) -> addDebugMsg errorMsg mainScene
        Right (Right scene') -> scene'
keyPress Escape s@MenuScene{menu} =
    case exitMenu menu of
        Nothing -> mainScene s
        Just x -> s{menu = x}


-- * changing of cursorStep

keyPress k s@EditorScene{} | k `member` cursorStepShortCuts =
    s{cursorStep = (cursorStepShortCuts ! k)}


-- * Debugging

keyPress D scene = inner scene
  where
    inner = id
--         addDebugMsg (pp $ fmap (getName . loadedSpritedName) $ availables scene)
{-        addDebugMsg (unwords $ map (getName . loadedSpritedName . eObjectSprited) $
                I.toList $ mainLayerIndexable $ objects scene)
        >>> addDebugMsg (show $ map associatedRobots $ filter isETerminal $
            I.toList $ mainLayerIndexable $ objects scene)
        >>> addDebugMsg (show $
            getName <$> loadedSpritedName <$> eObjectSprited <$> getSelectedObject scene)-}
--         addDebugMsg "c" (cursor s) >>>
--         addDebugMsg "os" (map eObjectPosition $ filter hasPosition $ I.toList $ objects s) >>>
--         addDebugMsg "sel" (getSelectedObject s)
--        chainApp (addDebugMsg "obj") (fmap (show) (objects s))
--        addDebugMsg "pos" (cursor s) .>>
--        addDebugMsg "getsel" (getSelectedObject s) .>>
--        addDebugMsg "sel" (selected s)

keyPress _ s = s
-- keyPress x s = es "keyPress" (x, getSelectedObject s)


-- | contains the shortcuts to change the vector the cursor is moved by the arrow keys.
cursorStepShortCuts :: Map Key (EditorScene -> EditorPosition)
cursorStepShortCuts = fromList (
    (K0, fromSelectedPixmap) -- like the selected object
    : map (modifySnd (\ x -> const $ EditorPosition x x)) constSquareShortcuts)
  where
    fromSelectedPixmap :: EditorScene -> EditorPosition
    fromSelectedPixmap EditorScene{sorts} =
        let (Size x y) = size $ getSelected sorts
        in EditorPosition x y
    -- | shortcuts that put cursorStep to a constant square
    constSquareShortcuts :: [(Key, Double)]
    constSquareShortcuts = [
        (K1, 1),
        (K2, fromUber 1),
        (K3, fromUber 4),
        (K4, fromUber 8),  -- 32
        (K5, fromUber 16), -- 64
        (K6, fromUber 32)  -- 128
      ]



topLevelMenu :: EditorScene -> Menu MenuLabel EditorScene
topLevelMenu s = mkMenu (mkLabel "Menu") [
    tileSelection s,
    layerMenu s,
    quit
  ]






