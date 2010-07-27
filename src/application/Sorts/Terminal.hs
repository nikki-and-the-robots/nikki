{-# language NamedFieldPuns, MultiParamTypeClasses, ScopedTypeVariables,
     ViewPatterns, DeriveDataTypeable, Rank2Types, FlexibleInstances #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Sorts.Terminal (
    sorts,
    unwrapTerminal,
    Terminal(exitMode),
    hasTerminalShape,
    ExitMode(..),
    OEMState(..),
  ) where


import Safe

-- import Data.Map (Map, (!), fromList)
-- import Data.List
import Data.Abelian
import Data.Indexable (Index)
import Data.Dynamic
import Data.Initial
import Data.Color

import Control.Monad

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Paths
import Utils

import Base.Events
import Base.Constants
import Base.Animation
import Base.Pixmap

import Object hiding (OEMState)

import Editor.Scene.Types hiding (sorts, ControlData, selected)
import Editor.Scene.Rendering
import Editor.Scene.Rendering.Helpers


-- * terminal configuration

blinkenLightSpeed :: Seconds
blinkenLightSpeed = 0.5

blinkSelectedLightSpeed :: Seconds
blinkSelectedLightSpeed = 0.2


sorts :: IO [Sort_]
sorts = do
    blinkenLights <- mapM (fromPure toPngPath >>>> getDataFileName >>>> loadPixmap 1) [
        "terminal-main_00",
        "terminal-main_01",
        "terminal-main_02",
        "terminal-main_03"
      ]
    littleColors <- readColorLights (\ color -> toPngPath ("terminal-" ++ color))
    let r = TSort (Pixmaps blinkenLights littleColors)
    return [Sort_ r]

toPngPath name = pngDir </> "terminals" </> name <.> "png"

data Pixmaps = Pixmaps {
    blinkenLights :: [Pixmap],
    littleColorLights :: ColorLights Pixmap
  }
    deriving Show

data ColorLights a = ColorLights {
    red_, blue_, green_, yellow_ :: a
  }
    deriving Show

colorLightList :: ColorLights a -> [a]
colorLightList (ColorLights a b c d) = [a, b, c, d]

mkColorLights :: [a] -> ColorLights a
mkColorLights [a, b, c, d] = ColorLights a b c d

readColorLights :: (String -> FilePath) -> IO (ColorLights Pixmap)
readColorLights f = do
    [r, b, g, y] <- mapM (
        fromPure f >>>> 
        getDataFileName >>>>
        loadPixmap 1
      ) ["red", "blue", "green", "yellow"]
    return $ ColorLights r b g y

instance PP (ColorLights Bool) where
    pp cl = map inner $ colorLightList cl
      where
        inner True = '|'
        inner False = 'o'

data ExitMode
    = DontExit
    | ExitToNikki
    | ExitToRobot Index
  deriving Show

data TSort = TSort {
    pixmaps :: Pixmaps
  }
    deriving (Show, Typeable)

data Terminal = Terminal {
    tchipmunk :: Chipmunk,
    robots :: [Index],
    selected :: Selected,
    exitMode :: ExitMode,
    lightState :: ColorLights Bool,
    selectedChangedTime :: Seconds
  }
    deriving (Show, Typeable)

data Selected
    = NikkiSelected {selectedRobotIndex :: Int}
    | RobotSelected {selectedRobotIndex :: Int}
  deriving Show

isNikkiSelected :: Selected -> Bool
isNikkiSelected (NikkiSelected _) = True
isNikkiSelected (RobotSelected _) = False

unwrapTerminal :: Object_ -> Maybe Terminal
unwrapTerminal (Object_ sort o) = cast o


instance Sort TSort Terminal where
    sortId = const $ SortId "terminal"
    size = pixmaps >>> blinkenLights >>> head >>> pixmapSize
    sortRender sort =
        sortRenderSinglePixmap (head $ blinkenLights $ pixmaps sort) sort

    objectEditMode _ = Just editMode

    initialize sort (Just space) editorPosition (Just state_) = do
        let pixmap = head $ blinkenLights $ pixmaps sort
            oemState = readNote "Terminal.initialize" state_
            attached = case oemState of
                NoRobots -> []
                Robots _ _ x -> x
            pos = qtPosition2Vector
                (editorPosition2QtPosition sort editorPosition)
                +~ baryCenterOffset
            bodyAttributes = StaticBodyAttributes{
                CM.position = pos
              }
            shapeAttributes = ShapeAttributes{
                elasticity = 0.8,
                friction = 2,
                CM.collisionType = TerminalCT
              }
            (polys, baryCenterOffset) = mkPolys $ size sort
            polysAndAttributes = map (tuple shapeAttributes) polys
        chip <- initChipmunk space bodyAttributes polysAndAttributes baryCenterOffset
        return $ Terminal chip attached (NikkiSelected 0) DontExit
                    (mkColorLights $ map (< length attached) [0..3])
                    0

    chipmunk = tchipmunk

    startControl t = t{exitMode = DontExit}

    updateNoSceneChange terminal now contacts (False, cd) =
        return $ blinkSelectedColorLight now terminal
    updateNoSceneChange terminal now contacts (True, cd) = do
        let cls = pp $ lightState terminal
        case selected terminal of
            NikkiSelected _ -> putStrLn ("[Nikki]\n " ++ cls)
            RobotSelected _ -> putStrLn (" Nikki\n[" ++ cls ++ "]")
        return $ blinkSelectedColorLight now $ controlTerminal now cd terminal

    render terminal sort ptr offset seconds =
        renderTerminal ptr offset seconds terminal sort


mkPolys :: Size Double -> ([ShapeType], Vector)
mkPolys (Size w h) =
    ([rect], baryCenterOffset)
  where
    rect =
        Polygon [
            Vector (- wh) (- hh),
            Vector (- wh) hh,
            Vector wh hh,
            Vector wh (- hh)
          ]
    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh


-- * controlling

controlTerminal :: Seconds -> ControlData -> Terminal -> Terminal
controlTerminal now cd t | Press BButton `elem` pushed cd =
--     || Press AButton `elem` pushed cd =
        case selected t of
            NikkiSelected _ -> t{exitMode = ExitToNikki}
            RobotSelected i -> t{exitMode = ExitToRobot (robots t !! i)}
controlTerminal now cd t | Press RightButton `elem` pushed cd =
    modifySelected now t (+ 1)
controlTerminal now cd t | Press LeftButton `elem` pushed cd =
    modifySelected now t (subtract 1)
controlTerminal now cd t@Terminal{selected = NikkiSelected i, robots}
    | Press DownButton `elem` pushed cd
      && not (null robots) =
        t{selected = RobotSelected i, selectedChangedTime = now}
controlTerminal now cd t@Terminal{selected = RobotSelected i}
    | Press UpButton `elem` pushed cd =
        t{selected = NikkiSelected i}
controlTerminal _ _ t = t

-- | changes the selected robot (if applicable)
-- and updates the selectedChangedTime (also if applicable)
modifySelected :: Seconds -> Terminal -> (Int -> Int) -> Terminal
modifySelected now t f =
    case selected t of
        NikkiSelected i -> t
        RobotSelected i -> t{
            selected = RobotSelected (clip (0, length (robots t) - 1) (f i)),
            selectedChangedTime = now
          }

blinkSelectedColorLight :: Seconds -> Terminal -> Terminal
blinkSelectedColorLight now t =
    t{lightState = lightState'}
  where
    lightState' = mkColorLights $ map p [0..3]
    p n = n < length (robots t)
        && (isNikkiSelected (selected t)
            || selectedRobotIndex (selected t) /= n
            || isBlinkTime)
    isBlinkTime =
        pickAnimationFrame [False, True] [blinkSelectedLightSpeed] now

-- initialTerminal :: Qt.Position Double -> a -> [Index] -> Object_ a Vector
-- initialTerminal p s i =
--     initialTerminalLights $
--         Terminal s (positionToVector p)
--             (State (length i) i 0 False [] 0 UninitializedAnimation)
-- 
-- 

-- 
-- initAnimation :: Object -> Object
-- initAnimation =
--     modifyTerminalState (modifyAnimation (const animation))
--   where
--     animation = mkAnimation AnimatedFrameSetType (const inner) 0
--     inner = AnimationPhases $ zip
--         (cycle [0 .. 3])
--         (repeat 0.6)
-- 
-- update :: Scene -> Seconds -> (Bool, ControlData)
--     -> Object -> IO Object
-- update scene now cd =
--     pure (modifyTerminalState (control scene cd)) >=>
--     pure (modifyTerminalState (updateState scene now))
-- 
-- control :: Scene -> (Bool, ControlData) -> State -> State
-- control scene (False, _) state = state
-- control scene (_, cd) state =
--     case mf of
--         Nothing -> state
--         Just f | not (isRobotSelected state) ->
--             modifySelected (const 0) $
--             state{isRobotSelected = True}
--         Just f ->
--             modifySelected f state
--   where
--     right = Press RightButton `elem` pushed cd
--     left = Press LeftButton `elem` pushed cd
--     mf = if right then
--         Just (+ 1)
--       else if left then
--         Just (subtract 1)
--       else
--         Nothing
-- 
-- 
-- updateState :: Scene -> Seconds
--     -> State -> State
-- updateState scene now t@State{terminalLastBlink, isRobotSelected} =
--     modifyMainAnimation now $
--     if shouldSwap then
--         swapSelectedBlinkStatus $ t{terminalLastBlink = now}
--       else
--         t
--   where
--     shouldSwap = stateToBlink && isRobotSelected && timeToBlink
--     timeToBlink = now - terminalLastBlink > blinkLength
--     stateToBlink = isRobotMode scene || isTerminalMode scene
-- 
-- 
-- modifyMainAnimation :: Seconds -> State -> State
-- modifyMainAnimation now = modifyAnimation inner
--   where
--     inner a = updateAnimation now AnimatedFrameSetType a
-- 
-- 
-- blinkLength :: Seconds
-- blinkLength = 0.5
-- 
-- -- | swaps the status of the selected TerminalLight.
-- swapSelectedBlinkStatus :: State -> State
-- swapSelectedBlinkStatus t@State{terminalSelected, terminalLights, terminalLength} =
--     let selected = allTerminalLights !! terminalSelected
--         initialLights = take terminalLength allTerminalLights
--     in if selected `elem` terminalLights then
--         -- switch off
--         t{terminalLights = delete selected initialLights}
--       else
--         -- switch on
--         t{terminalLights = initialLights}

-- * game rendering

renderTerminal :: Ptr QPainter -> Offset Double -> Seconds -> Terminal
    -> TSort -> IO ()
renderTerminal ptr offset seconds t sort = do
    renderTerminalBackground ptr offset seconds t sort
    renderLittleColorLights ptr offset t sort

renderTerminalBackground ptr offset now t sort = do
    let pixmap =
            pickAnimationFrame (blinkenLights $ pixmaps sort)
                [blinkenLightSpeed] now
    renderChipmunk ptr offset pixmap (tchipmunk t)


renderLittleColorLights ptr offset t sort = do
    pos <- fst <$> getRenderPosition (tchipmunk t)
    mapM_ (renderLight ptr (offset +~ pos) t (littleColorLights $ pixmaps sort))
        [red_, blue_, green_, yellow_]

renderLight :: Ptr QPainter -> Offset Double -> Terminal -> ColorLights Pixmap
    -> (forall a . (ColorLights a -> a)) -> IO ()
renderLight ptr offset t pixmaps color =
    when (color $ lightState t) $ do
        let lightOffset = color littleLightOffsets
            pixmap = color pixmaps
        renderPixmap ptr offset lightOffset Nothing pixmap
--         resetMatrix ptr
--         translate ptr offset
--         translate ptr lightOffset
--         drawPixmap ptr zero pixmap

littleLightOffsets :: ColorLights (Offset Double)
littleLightOffsets = ColorLights {
    red_ = Position redX lightsY,
    blue_ = Position blueX lightsY,
    green_ = Position greenX lightsY,
    yellow_ = Position yellowX lightsY
  }

redX, blueX, greenX, yellowX, lightsY :: Double
redX = redBoxX - glowDist
blueX = blueBoxX - glowDist
greenX = greenBoxX - glowDist
yellowX = yellowBoxX - glowDist

lightsY = boxY - glowDist

glowDist, boxWidth, padding :: Double
glowDist = 12
boxWidth = 12
padding = 8

redBoxX, blueBoxX, greenBoxX, yellowBoxX, boxY :: Double
redBoxX = 28
blueBoxX = redBoxX + boxWidth + padding
greenBoxX = blueBoxX + boxWidth + padding
yellowBoxX = greenBoxX + boxWidth + padding

boxY = fromUber 7




--     renderLights ptr (offset +~ pos) t

-- renderTerminal :: Ptr QPainter -> Qt.Position Double -> Object -> IO (Qt.Position Double)
-- renderTerminal ptr offset (Terminal sprited chipmunk state) = do
--     resetMatrix ptr
--     translate ptr offset
--     pos <- (vectorToPosition . fst) <$> getRenderPosition chipmunk
--     translate ptr pos
--     let pixmap = animationPixmap (terminalAnimation state) sprited
--     drawPixmap ptr zero pixmap
--     return pos
-- 
-- renderLights :: Ptr QPainter -> Qt.Position Double -> Terminal -> Sort -> IO ()
-- renderLights ptr offset scene (Terminal _ _ state) = do
--     let lightPixmaps = searchLightPixmaps scene
--     mapM_ (uncurry $ drawLights ptr offset) $
--         map (coloredLights lightPixmaps !) $
--             terminalLights state


-- -- * custom rendering
-- 
-- -- | draws a single light on top of the in game terminal
-- drawLights :: Ptr QPainter -> Qt.Position Double -> Double -> Ptr QPixmap -> IO ()
-- drawLights ptr offset x pixmap = do
--     resetMatrix ptr
--     translate ptr (Position x lightsY +~ offset)
--     drawPixmap ptr zero pixmap
-- 
-- 
-- searchLightPixmaps :: Scene -> (Ptr QPixmap, Ptr QPixmap, Ptr QPixmap, Ptr QPixmap)
-- searchLightPixmaps scene =
--     (getPixmap "terminal-red",
--      getPixmap "terminal-blue",
--      getPixmap "terminal-green",
--      getPixmap "terminal-yellow")
--   where
--     m = osdSpriteds scene
--     getPixmap :: String -> Ptr QPixmap
--     getPixmap name = defaultPixmap (sprited (m ! name))
-- 
-- coloredLights :: (a, a, a, a) -> Map TerminalLight (Double, a)
-- coloredLights (red, blue, green, yellow) = fromList [
--     (TerminalRed, (redX, red)),
--     (TerminalBlue, (blueX, blue)),
--     (TerminalGreen, (greenX, green)),
--     (TerminalYellow, (yellowX, yellow))
--   ]
-- 
-- 
-- 
-- 
-- 
-- 


-- * special edit mode (OEM)
-- how to attach robots to Terminals

editMode :: ObjectEditMode
editMode = ObjectEditMode {
    oemInitialState = show (initial :: OEMState),
    oemEnterMode = \ scene_ state_ ->
        case fromDynamic scene_ of
            Just scene -> show $ enterMode scene
                (readNote "Terminal.editMode.oemEnterMode" state_),
    oemUpdate = \ scene_ key ->
        case fromDynamic scene_ of
            Just scene ->
                readNote "Terminal.editMode.oemUpdate" >>> editorUpdate scene key >>> show,
    oemRender = \ ptr scene_ 
        (readNote  "Terminal.editMode.oemRender" -> state :: OEMState) ->
            case fromDynamic scene_ of
                Just scene -> oemRender_ ptr scene state
  }

data OEMState
    = NoRobots
    | Robots {
        availableRobots :: [Index], -- INV: not null
        selectedRobot :: Index,
        attachedRobots :: [Index]
      }
  deriving (Read, Show)

instance Initial OEMState where
    initial = NoRobots

enterMode :: EditorScene -> OEMState -> OEMState
enterMode scene NoRobots =
    case getRobotIndices scene of
        [] -> NoRobots
        available@(first : _) -> Robots available first []
enterMode scene (Robots _ selected attached) =
    case getRobotIndices scene of
        [] -> NoRobots
        available@(first : _) ->
            Robots available selected' (filter (`elem` available) attached)
          where
            selected' = if selected `elem` available then selected else first

editorUpdate :: EditorScene -> Key -> OEMState -> OEMState
editorUpdate scene key NoRobots = NoRobots
editorUpdate scene key state@(Robots available selected attached) =
  case key of
    RightArrow -> state{selectedRobot = searchNext selected available}
    LeftArrow  -> state{selectedRobot = searchNext selected (reverse available)}
    Enter -> state{attachedRobots = swapIsElem selected attached}
    _ -> state

-- | searches the next element that is not equal to the given one in the list
-- wraps the list around.
searchNext :: Eq e => e -> [e] -> e
searchNext needle list | needle `elem` list =
    dropWhile (/= needle) (cycle list)
    |> (!! 1)

-- | removes the given element, is it's element of the list,
-- adds it otherwise (at the end of the list)
swapIsElem :: Eq e => e -> [e] -> [e]
swapIsElem needle list | needle `elem` list = filter (/= needle) list
swapIsElem needle list = list +: needle


-- * rendering of OEM

oemRender_ :: Ptr QPainter -> EditorScene -> OEMState -> IO ()
oemRender_ ptr scene state = do
    offset <- transformation ptr (cursor scene) (getCursorSize scene)
    renderObjectScene ptr offset scene
    renderOEMOSDs ptr offset scene state

renderOEMOSDs :: Ptr QPainter -> Offset Double -> EditorScene -> OEMState -> IO ()
renderOEMOSDs ptr offset scene NoRobots = return ()
renderOEMOSDs ptr offset scene (Robots _ selected attached) = do
    renderRobotBox orange{alphaC = 0.5} (getMainObject scene selected)
    mapM_ (renderRobotBox yellow{alphaC = 0.3}) $ map (getMainObject scene) $
        attached
  where
    renderRobotBox :: RGBA -> EditorObject -> IO ()
    renderRobotBox color robot = do
        let sort = editorSort robot
            pos = editorPosition2QtPosition sort $ editorPosition robot
            size_ = size sort
        drawColoredBox ptr (pos +~ offset) size_ 4 color

-- calculateRenderTransformationTerminal :: Ptr QPainter -> EditorScene -> IO (Position Double)
-- calculateRenderTransformationTerminal ptr scene@TerminalScene{mainScene} =
--     transformation ptr pos size
--   where
--     (pos, size) = case getTerminalMRobot scene of
--         -- use the terminal
--         Nothing -> (cursor mainScene, getCursorSize scene)
-- --         Just (ERobot (Position x y) sprited) ->
-- --             error "            (EditorPosition x (y + height size), size)"
-- --           where
-- --             size = defaultPixmapSize sprited



-- * game logick

hasTerminalShape :: Terminal -> Shape -> Bool
hasTerminalShape terminal shape =
    shape `elem` shapes (tchipmunk terminal)

-- whichTerminalCollides collisions =
--     let colls = filter snd $ toList $ terminals collisions
--     in case colls of
--         (a : r) -> fst a
-- 
-- 
-- -- collision setters
-- 
-- whichTerminal :: Shape -> Shape -> Shape
-- whichTerminal terminalShape _ = terminalShape
-- 
-- activateTerminal :: Indexable object -> Contacts o -> Shape -> IO (Contacts o)
-- activateTerminal objects collisions terminalShape = do
--     e "activateTerminal"
-- --     let terminalBody = getBody terminalShape
-- --     let terminal = single "activatTerminal" $ I.findIndices pred objects
-- --         pred :: object -> Bool
-- --         pred object = isTerminal (sort_ object) && (terminalBody == body (chipmunk_ object))
-- --     return $ setTerminalActive collisions terminal
-- 
-- setTerminalActive :: Contacts o -> Index -> Contacts o
-- setTerminalActive collisions@Contacts{terminals} i =
--     collisions{terminals = insert i True terminals}
-- 




