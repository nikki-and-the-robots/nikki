{-# language NamedFieldPuns, MultiParamTypeClasses, ScopedTypeVariables,
     ViewPatterns, DeriveDataTypeable, Rank2Types, FlexibleInstances #-}

module Sorts.Terminal (
    sorts,
    unwrapTerminal,
    Terminal,
    terminalExitMode,
    hasTerminalShape,
    ExitMode(..),
    renderTerminalOSD,
    OEMState(..),
  ) where


import Safe

import Data.Abelian
import Data.Indexable (Index)
import Data.Dynamic
import Data.Initial
import Data.Color
import Data.Traversable
import Data.Foldable (Foldable, foldMap, forM_)
import Data.Monoid

import Control.Monad (when)

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Paths
import Utils

import Base.Events
import Base.Constants
import Base.Animation
import Base.Pixmap
import Base.Types hiding (selected, OEMState, Mode(..))
import qualified Base.Types

import Object

import Editor.Scene.Types
import Editor.Scene.Rendering
import Editor.Scene.Rendering.Helpers


-- * terminal configuration

blinkenLightSpeed :: Seconds
blinkenLightSpeed = 0.5

blinkLength :: Seconds
blinkLength = 0.2


sorts :: IO [Sort_]
sorts = do
    blinkenLights <- fmapM (fromPure toPngPath >>>> getDataFileName >>>> loadPixmap 1) [
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

instance Functor ColorLights where
    fmap f (ColorLights a b c d) = ColorLights (f a) (f b) (f c) (f d)

instance Foldable ColorLights where
    foldMap f (ColorLights a b c d) = mconcat $ map f [a, b, c, d]

instance Traversable ColorLights where
    traverse cmd (ColorLights a b c d) =
        ColorLights <$> cmd a <*> cmd b <*> cmd c <*> cmd d

toList :: ColorLights a -> [a]
toList (ColorLights a b c d) = [a, b, c, d]

fromList :: [a] -> ColorLights a
fromList [a, b, c, d] = ColorLights a b c d

fzipWith :: (a -> b -> c) -> ColorLights a -> ColorLights b -> ColorLights c
fzipWith f (ColorLights a b c d) (ColorLights p q r s) =
    ColorLights (f a p) (f b q) (f c r) (f d s)

readColorLights :: (String -> FilePath) -> IO (ColorLights Pixmap)
readColorLights f =
    fmapM (fromPure f >>>> getDataFileName >>>> loadPixmap 1) $
        ColorLights "red" "blue" "green" "yellow"

instance PP (ColorLights Bool) where
    pp cl = map inner $ toList cl
      where
        inner True = '|'
        inner False = 'o'


data TSort = TSort {
    pixmaps :: Pixmaps
  }
    deriving (Show, Typeable)

data Terminal = Terminal {
    chipmunk :: Chipmunk,
    robots :: [Index],
    state :: State
  }
    deriving (Show, Typeable)

unwrapTerminal :: Object_ -> Maybe Terminal
unwrapTerminal (Object_ sort o) = cast o

terminalExitMode :: Terminal -> ExitMode
terminalExitMode = state >>> exitMode


data State
    = State {
        gameMode :: GameMode,
        row :: MenuRow,
        robotIndex :: Int,
        changedTime :: Seconds,
        exitMode :: ExitMode
      }
  deriving Show

data MenuRow = NikkiRow | RobotRow
  deriving Show

initialMenuState :: Seconds -> State
initialMenuState now = State NikkiMode RobotRow 0 now DontExit

isNikkiSelected :: State -> Bool
isNikkiSelected (State _ NikkiRow _ _ _) = True
isNikkiSelected (State _ RobotRow _ _ _) = False

-- | resets the terminal state, when it is started to be used.
reset :: Seconds -> [Index] -> State -> State
reset t robots (State _ _ i _ _) =
    State TerminalMode row i t DontExit
  where
    row = if null robots then NikkiRow else RobotRow

blinkenLightsState :: Seconds -> [Index] -> State -> ColorLights Bool
blinkenLightsState now robots state =
    case row state of
        NikkiRow -> full
        RobotRow -> if blinkingOut then fzipWith (\ f s -> f && not s) full selected else full
  where
    full = ColorLights (l > 0) (l > 1) (l > 2) (l > 3)
    selected = ColorLights (i == 0) (i == 1) (i == 2) (i == 3)
    i = robotIndex state
    l = length robots
    blinkingOut = blinkingMode && even (floor ((now - changedTime state) / blinkLength))
    blinkingMode = case gameMode state of
        NikkiMode -> False
        _ -> True

-- | changes the selected robot (if applicable)
-- and updates the selectedChangedTime (also if applicable)
modifySelected :: Seconds -> [Index] -> (Int -> Int) -> State -> State
modifySelected now robots f state =
    case row state of
        NikkiRow -> state
        RobotRow -> state{robotIndex = normalize (f (robotIndex state)), changedTime = now}
  where
    normalize = clip (0, length robots - 1)


data GameMode = NikkiMode | TerminalMode | RobotMode
  deriving Show


data ExitMode
    = DontExit
    | ExitToNikki
    | ExitToRobot Index
  deriving Show


-- * Sort implementation

instance Sort TSort Terminal where
    sortId = const $ SortId "terminal"
    size = pixmaps >>> blinkenLights >>> head >>> pixmapSize
    sortRender sort ptr _ =
        renderPixmapSimple ptr (head $ blinkenLights $ pixmaps sort)

    objectEditModeMethods _ = Just editMode

    initialize sort (Just space) editorPosition (Just state_) = do
        let oemState = readNote "Terminal.initialize" state_
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
            polysAndAttributes = map (mkShapeDescription shapeAttributes) polys
        chip <- initChipmunk space bodyAttributes polysAndAttributes baryCenterOffset
        return $ Terminal chip attached (initialMenuState 0)

    immutableCopy t =
        CM.immutableCopy (chipmunk t) >>= \ x -> return t{chipmunk = x}

    getControlledChipmunk = chipmunk

    chipmunks = chipmunk >>> return

    startControl now t = t{state = reset now (robots t) (state t)}

    updateNoSceneChange sort now contacts (False, cd) terminal =
        return terminal
    updateNoSceneChange sort now contacts (True, cd) terminal =
        return terminal{state = updateState now cd (robots terminal) (state terminal)}

    render terminal sort ptr offset now =
        renderTerminal ptr offset now terminal sort


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

updateState :: Seconds -> ControlData -> [Index] -> State -> State
updateState now cd robots state
    | Press BButton `elem` pressed cd
    || Press AButton `elem` pressed cd =
    -- exit terminal mode
        case row state of
            NikkiRow -> state{exitMode = ExitToNikki, robotIndex = 0}
            RobotRow -> state{exitMode = ExitToRobot (robots !! robotIndex state)}
updateState now cd robots state | Press RightButton `elem` pressed cd =
    -- go right in robot list
    modifySelected now robots (+ 1) state
updateState now cd robots state | Press LeftButton `elem` pressed cd =
    -- go left in robot list
    modifySelected now robots (subtract 1) state
updateState now cd robots state@State{row = NikkiRow}
    | Press UpButton `elem` pressed cd
    -- select to robot list
      && not (null robots) =
        state{row = RobotRow}
updateState now cd robots state@State{row = RobotRow}
    | Press DownButton `elem` pressed cd =
    -- select exit (nikki) menu item (go down)
        state{row = NikkiRow}
updateState _ _ _ t = t


-- * game rendering

renderTerminal :: Ptr QPainter -> Offset Double -> Seconds -> Terminal
    -> TSort -> IO ()
renderTerminal ptr offset now t sort = do
    renderTerminalBackground ptr offset now t sort
    renderLittleColorLights ptr offset now t sort

-- | renders the main terminal pixmap (with blinkenlights)
renderTerminalBackground ptr offset now t sort = do
    let pixmap =
            pickAnimationFrame (blinkenLights $ pixmaps sort)
                [blinkenLightSpeed] now
    renderChipmunk ptr offset pixmap (chipmunk t)

-- | renders the little colored lights (for the associated robots) on the terminal in the scene
renderLittleColorLights ptr offset now t sort = do
    pos <- fst <$> getRenderPosition (chipmunk t)
    let colorStates = blinkenLightsState now (robots t) (state t)
    mapM_ (renderLight ptr (offset +~ pos) (littleColorLights $ pixmaps sort) colorStates)
        [red_, blue_, green_, yellow_]

renderLight :: Ptr QPainter -> Offset Double -> ColorLights Pixmap -> ColorLights Bool
    -> (forall a . (ColorLights a -> a))
    -> IO ()
renderLight ptr offset pixmaps colorStates color =
    when (color colorStates) $ do
        let lightOffset = color littleLightOffsets
            pixmap = color pixmaps
        renderPixmap ptr offset lightOffset Nothing Nothing pixmap

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


renderTerminalOSD :: Ptr QPainter -> Seconds -> Scene Object_ -> IO ()
renderTerminalOSD ptr now scene@Scene{mode = Base.Types.TerminalMode{Base.Types.terminal}} = do
    let Just t = unwrapTerminal $ getMainlayerObject scene terminal
        cls = pp $ blinkenLightsState now (robots t) (state t)
        texts = case row $ state t of
            NikkiRow -> cls : "[BACK]" : []
            RobotRow -> ("[" ++ cls ++ "]") : " BACK" : []
    resetMatrix ptr
    setPenColor ptr 255 255 255 255 1
    let textSize = Size 250 50
    setFontSize ptr (round (height textSize))
    windowSize <- fmap fromIntegral <$> sizeQPainter ptr
    translate ptr ((sizeToPosition (fmap (/ 2) (windowSize -~ textSize))) +~ Position 0 300)
    forM_ texts $ \ text -> do
        drawText ptr (Position 10 0) False text
        translate ptr (Position 0 (height textSize))
renderTerminalOSD _ _ _ = return ()


-- * special edit mode (OEM)
-- how to attach robots to Terminals

editMode :: ObjectEditModeMethods Sort_
editMode = ObjectEditModeMethods {
    oemInitialState = \ _ -> show (initial :: OEMState),
    oemEnterMode = \ scene state_ ->
        show $ enterMode scene
            (readNote "Terminal.editMode.oemEnterMode" state_),
    oemUpdate = \ scene key ->
        readNote "Terminal.editMode.oemUpdate" >>> editorUpdate scene key >>> show,
    oemRender = \ ptr scene
        (readNote  "Terminal.editMode.oemRender" -> state :: OEMState) ->
            oemRender_ ptr scene state
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

enterMode :: EditorScene Sort_ -> OEMState -> OEMState
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

editorUpdate :: EditorScene Sort_ -> AppButton -> OEMState -> OEMState
editorUpdate scene key NoRobots = NoRobots
editorUpdate scene key state@(Robots available selected attached) =
  case key of
    RightButton -> state{selectedRobot = searchNext selected available}
    LeftButton -> state{selectedRobot = searchNext selected (reverse available)}
    AButton -> state{attachedRobots = swapIsElem selected attached}
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

oemRender_ :: Ptr QPainter -> EditorScene Sort_ -> OEMState -> IO ()
oemRender_ ptr scene state = do
    offset <- transformation ptr (cursor scene) (getCursorSize scene)
    renderObjectScene ptr offset scene
    renderOEMOSDs ptr offset scene state

renderOEMOSDs :: Ptr QPainter -> Offset Double -> EditorScene Sort_ -> OEMState -> IO ()
renderOEMOSDs ptr offset scene NoRobots = return ()
renderOEMOSDs ptr offset scene (Robots _ selected attached) = do
    renderRobotBox orange{alphaC = 0.5} (getMainObject scene selected)
    mapM_ (renderRobotBox yellow{alphaC = 0.3}) $ map (getMainObject scene) $
        attached
  where
    renderRobotBox :: RGBA -> EditorObject Sort_ -> IO ()
    renderRobotBox color robot = do
        let sort = editorSort robot
            pos = editorPosition2QtPosition sort $ editorPosition robot
            size_ = size sort
        drawColoredBox ptr (pos +~ offset) size_ 4 color


-- * game logick

hasTerminalShape :: Terminal -> Shape -> Bool
hasTerminalShape terminal shape =
    shape `elem` shapes (chipmunk terminal)
