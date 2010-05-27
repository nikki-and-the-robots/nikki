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

import Utils
import Base.Constants

import Data.Maybe
import Data.Map hiding (map, filter, mapMaybe)
import Data.List
import Data.SelectTree
import qualified Data.Indexable as I
import Data.Indexable (Index, (>:))
import Control.Monad.FunctorM
import Data.Menu hiding (selected)
import Data.Abelian
-- import Data.Generics.Uniplate.Data

import Control.Monad.State
import Control.Applicative ((<$>))

import Graphics.Qt

import System.FilePath

import Base.Grounds

import Base.Sprited

import Object.Types
import qualified Sorts.Nikki
import qualified Sorts.Terminal
import qualified Sorts.Tiles
import qualified Sorts.Robots.Jetpack

import Editor.Scene.Types
import Editor.Scene.Menu as Menu
import Editor.Scene.Rendering


-- | looks, if there is an object under the cursor (and therefore selected)
-- in the selected layer
searchSelectedObject :: EditorScene -> Maybe Index
searchSelectedObject s@EditorScene{selectedLayer} =
    let indices = I.findIndices isSelected (content (objects s !|| selectedLayer))
        isSelected o = lowerCorner o == cursor s
        lowerCorner o = fst o
    in case indices of
        [] -> Nothing
        ll -> Just $ last ll

-- * normalizers

normSelected :: EditorScene -> EditorScene
normSelected s@EditorScene{} = s{selected = searchSelectedObject s}
normSelected x = x

-- * constructors

-- constructs initial objects out of sprites and the cursor position
mkEditorObject :: Position Double -> Sprited -> EditorObject
{-mkEditorObject p s | spritedIsSomething s "terminals" (Name "terminal-main") =
    ETerminal (leftLower2leftUpper s p) s []
mkEditorObject p s | spritedIsSomething s "nikki" (Name "nikki") =
    ENikki (leftLower2leftUpper s p) s
mkEditorObject p s | spritedInDirectory s "robots" =
    ERobot (leftLower2leftUpper s p) s
mkEditorObject p s | spritedIsSomething s "tiles/architecture" (Name "milky") =
    EMilkMachine (leftLower2leftUpper s p) s
mkEditorObject p s | any (spritedInDirectory s) ["tiles", "backgrounds", "multilayers"] =
    ETile (leftLower2leftUpper s p) s
mkEditorObject p s | spritedInDirectory s "objects" =
    EBox (leftLower2leftUpper s p) s-}
mkEditorObject p s = es "mkEditorObject" s

-- returns if a Sprited comes from the given directory (relative to pngDir)
spritedInDirectory :: Sprited -> FilePath -> Bool
spritedInDirectory s path = (pngDir </> path) `isPrefixOf` loadedSpritedDir s

-- returns if a sprited corresponds to a given path (from pngDir) and Name.
spritedIsSomething :: Sprited -> FilePath -> Name -> Bool
spritedIsSomething s path name =
    spritedInDirectory s path &&
    name == loadedSpritedName s

-- | the initial editor scene
initScene :: Maybe (String, Grounds PickleObject) -> IO EditorScene
initScene mObjects = flip evalStateT empty $ do

    availables <- liftIO lookupObjects >>= fmapM loadSprited
    availableBackgrounds <- liftIO lookupBackgrounds >>= fmapM loadSprited

    sorts <- liftIO $ getAllSorts

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
        debugMsgs = []
      }

getAllSorts :: IO (SelectTree Sort_)
getAllSorts = do
    sorts <- concat <$> mapM id sortLoaders
    return $ Node "editor-Tiles" (I.fromList $ map Leaf sorts) 0

sortLoaders :: [IO [Sort_]]
sortLoaders = [
    map mkSort_ <$> Sorts.Nikki.sorts,
    map mkSort_ <$> Sorts.Terminal.sorts,
    map mkSort_ <$> Sorts.Robots.Jetpack.sorts,
    map mkSort_ <$> Sorts.Tiles.sorts
  ]


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
    new = (cursor, selectedSort)
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
    let (mainLayer', f) = I.toBack i (mainLayerIndexable objects)
    in scene{objects = f objects{mainLayer = mkMainLayer mainLayer'}}


-- * Layers

keyPress Plus s@EditorScene{objects, selectedLayer} =
    s{selectedLayer = modifyGroundsIndex objects (+ 1) selectedLayer}
keyPress Minus s@EditorScene{objects, selectedLayer} =
    s{selectedLayer = modifyGroundsIndex objects (subtract 1) selectedLayer}



-- * TerminalScene

-- mode changing
keyPress Enter s@EditorScene{} = e "Enter"
-- | Just (ETerminal _ _ associated) <- getSelectedObject s =
--     TerminalScene s (getRobotIndices s) (fromJust $ selected s) associated []
keyPress Escape s@TerminalScene{} =
    e "TerminalScene"
--     (mainScene s){objects = newObjects}
--   where
--     newObjects = modifyMainLayer setTerminal oldObjects
--     setTerminal = modifyByIndex
--         (flip setAssociatedRobots (tmSelectedRobots s))
--         (tmTerminalIndex s)
--     oldObjects = objects $ mainScene s

keyPress RightArrow s@TerminalScene{tmAvailableRobots = rs} =
   s{tmAvailableRobots = Utils.rotate rs}
keyPress LeftArrow s@TerminalScene{tmAvailableRobots = rs} =
   s{tmAvailableRobots = Utils.rotateBack rs}
keyPress Space s@TerminalScene{tmAvailableRobots = (new : rrs)} =
    s{tmSelectedRobots = switchElement new oldList}
  where
    oldList = tmSelectedRobots s
    switchElement :: Eq a => a -> [a] -> [a]
    switchElement needle hay =
        if needle `elem` hay then
            filter (/= needle) hay
          else
            hay +: needle

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
        let (Size x y) = fmap fromIntegral $ size_ $ getSelected sorts
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






