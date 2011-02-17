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
    TerminalOEMState(..),
  ) where


import Safe

import Data.Abelian
import Data.Indexable (Index)
import Data.Generics
import Data.Traversable
import Data.Foldable (Foldable, foldMap)
import Data.Monoid

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt hiding (red, blue, green, yellow)
import qualified Graphics.Qt as Qt

import Utils

import Base hiding (Mode(..))
import qualified Base

import Object

import Sorts.Nikki.Configuration (nikkiSize)

import Editor.Scene.Types
import Editor.Scene.Rendering
import Editor.Scene.Rendering.Helpers


-- * terminal configuration

blinkenLightSpeed :: Seconds
blinkenLightSpeed = 0.5

blinkLength :: Seconds
blinkLength = 0.4


-- * sort loading

sorts :: RM [Sort_]
sorts = do
    let nameToPixmap offset =
            fromPure toPngPath >>>>
            getDataFileName >>>>
            loadPixmap (Position offset offset)
    backgroundPixmap <- (nameToPixmap (1 + fromUber 1)) "terminal-standard"
    displayBlinkenLights <- fmapM (nameToPixmap (1 - fromUber 13)) (
        "display_00" :
        "display_01" :
        "display_02" :
        "display_03" :
      [])
    littleColors <- readColorLights (\ color -> toPngPath ("terminal-" ++ color))
    osdPixmaps <- loadOsdPixmaps
    let r = TSort (Pixmaps backgroundPixmap displayBlinkenLights littleColors) osdPixmaps
    return [Sort_ r]

toPngPath name = pngDir </> "terminals" </> name <.> "png"

readColorLights :: (String -> FilePath) -> RM (ColorLights Pixmap)
readColorLights f =
    fmapM (fromPure f >>>> getDataFileName >>>> loadPixmap (Position 13 13)) $
        ColorLights "red" "blue" "green" "yellow"

loadOsdPixmaps :: RM OsdPixmaps
loadOsdPixmaps = do
    background <- removeUberPixelShadow <$>
                    (loadPixmap zero =<< toOsdPath "background")
    let colors = ColorLights "red" "blue" "green" "yellow"
        load :: Int -> Int -> String -> RM Pixmap
        load xOffset yOffset = toOsdPath >>>> loadPixmap (Position xOffset yOffset)
    centers <- fmapM (load 27 27) $ fmap (++ "-center") colors
    frames <- fmapM (load 27 27) $ fmap (++ "-frame") colors
    exitCenter <- (load 24 23) "exit-center"
    exitFrame <- (load 27 27) "exit-frame"
    return $ OsdPixmaps background centers frames exitCenter exitFrame
  where
    osdPath = pngDir </> "terminals" </> "osd"
    toOsdPath :: String -> RM FilePath
    toOsdPath name = getDataFileName (osdPath </> name <.> "png")
    -- removes the shadow to the right and bottom
    -- by decreasing the size
    removeUberPixelShadow :: Pixmap -> Pixmap
    removeUberPixelShadow p@Pixmap{pixmapSize} =
        p{pixmapSize = pixmapSize -~ fmap fromUber (Size 1 1)}


-- | type to bundle things for the four terminal colors: red, blue, green and yellow (in that order)
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

selectedColorLights :: Int -> ColorLights Bool
selectedColorLights i = ColorLights (i == 0) (i == 1) (i == 2) (i == 3)


data Pixmaps = Pixmaps {
    background :: Pixmap,
    blinkenLights :: [Pixmap],
    littleColorLights :: ColorLights Pixmap
  }
    deriving Show

data OsdPixmaps = OsdPixmaps {
    osdBackground :: Pixmap,
    osdCenters :: ColorLights Pixmap,
    osdFrames :: ColorLights Pixmap,
    osdExitCenter :: Pixmap,
    osdExitFrame :: Pixmap
  }
    deriving (Show, Typeable)


data TSort = TSort {
    pixmaps :: Pixmaps,
    osdPixmaps :: OsdPixmaps
  }
    deriving (Show, Typeable)

isTerminal :: Sort sort o => sort -> Bool
isTerminal (cast -> Just _ :: Maybe TSort) = True
isTerminal (cast -> Just (Sort_ inner) :: Maybe Sort_) = isTerminal inner
isTerminal _ = False

data Terminal = Terminal {
    chipmunk :: Chipmunk,
    robots :: [Index],
    state :: State
  }
    deriving (Show, Typeable)

unwrapTerminal :: Object_ -> Maybe Terminal
unwrapTerminal (Object_ sort o) = cast o

unwrapTerminalSort :: Sort_ -> Maybe TSort
unwrapTerminalSort (Sort_ sort) = cast sort

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
  deriving (Eq, Show)

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

blinkenLightsState :: Seconds -> [Index] -> State -> (ColorLights Bool, Bool)
blinkenLightsState now robots state =
    case row state of
        NikkiRow -> (full, not blinkingOut)
        RobotRow -> tuple
            (if blinkingOut then fzipWith (\ f s -> f && not s) full selected else full)
            True
  where
    full = ColorLights (l > 0) (l > 1) (l > 2) (l > 3)
    selected = selectedColorLights i
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
        RobotRow -> if newIndex /= robotIndex state
                    then state{robotIndex = newIndex, changedTime = now}
                    else state -- don't reset changedTime when nothing changed
  where
    normalize = clip (0, length robots - 1)
    newIndex = normalize (f (robotIndex state))


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
    size = const $ Size (fromUber 48) (fromUber 48)
    sortRender sort ptr _ _ =
        renderPixmapSimple ptr $ background $ pixmaps sort

    objectEditMode _ = Just oemMethods

    initialize sort (Just space) editorPosition (Just (OEMState oemState_)) = do
        let Just oemState :: Maybe TerminalOEMState = cast oemState_
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

    getControlledChipmunk scene _ =
        getControlledChipmunk scene $
        getMainlayerObject scene (Base.nikki $ mode scene)

    chipmunks = chipmunk >>> return

    startControl now t = t{state = reset (now - blinkLength) (robots t) (state t)}

    updateNoSceneChange sort mode now contacts (False, cd) terminal =
        return terminal
    updateNoSceneChange sort mode now contacts (True, cd) terminal =
        return terminal{state = updateState now cd (robots terminal) (state terminal)}

    render terminal sort ptr offset now = do
        pos <- fst <$> getRenderPosition (chipmunk terminal)
        renderTerminal sort ptr offset now terminal pos


mkPolys :: Size Double -> ([ShapeType], Vector)
mkPolys size =
    ([rect], baryCenterOffset)
  where
    rect =
        Polygon [
            Vector (- wh + padding) (- hh),
            Vector (- wh + padding) hh,
            Vector (wh - padding) hh,
            Vector (wh - padding) (- hh)
          ]
    (Size wh hh) = fmap (/ 2) size
    baryCenterOffset = Vector wh hh
    -- the physics object is much narrower to simulate,
    -- nikki being able to use the terminal if nikki is completely
    -- inside the terminal frame. (With height, there is no such padding,
    -- as nikki can only jump when standing on the ground.)
    padding = width nikkiSize


-- * controlling

-- | controls the terminal in terminal mode
updateState :: Seconds -> ControlData -> [Index] -> State -> State
updateState now cd robots state | Press AButton `elem` pressed cd =
  case row state of
    NikkiRow -> exitToNikki state
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
        state{row = RobotRow, changedTime = now}
updateState now cd robots state@State{row = RobotRow}
    | Press DownButton `elem` pressed cd =
    -- select exit (nikki) menu item (go down)
        state{row = NikkiRow, changedTime = now}
updateState _ _ _ t = t

exitToNikki :: State -> State
exitToNikki state = state{exitMode = ExitToNikki, robotIndex = 0}


-- * game rendering

renderTerminal :: TSort -> Ptr QPainter -> Offset Double -> Seconds
    -> Terminal -> Qt.Position Double
    -> IO ()
renderTerminal sort ptr offset now t pos = do
    renderTerminalBackground sort ptr offset pos
    renderDisplayBlinkenLights sort ptr offset now t pos
    renderLittleColorLights sort ptr offset now t pos

renderTerminalBackground sort ptr offset pos = do
    renderPixmap ptr offset pos Nothing $ background $ pixmaps sort

-- | renders the main terminal pixmap (with blinkenlights)
renderDisplayBlinkenLights sort ptr offset now t pos = do
    let pixmap = pickAnimationFrame (blinkenLights $ pixmaps sort)
                 [blinkenLightSpeed] now
    renderPixmap ptr offset pos Nothing pixmap

-- | renders the little colored lights (for the associated robots) on the terminal in the scene
renderLittleColorLights sort ptr offset now t pos = do
    let colorStates = fst $ blinkenLightsState now (robots t) (state t)
    mapM_
        (renderLight ptr (offset +~ pos) (littleColorLights $ pixmaps sort) colorStates)
        [red_, blue_, green_, yellow_]

renderLight :: Ptr QPainter -> Offset Double -> ColorLights Pixmap -> ColorLights Bool
    -> (forall a . (ColorLights a -> a))
    -> IO ()
renderLight ptr offset pixmaps colorStates color =
    when (color colorStates) $ do
        let lightOffset = color littleLightOffsets
            pixmap = color pixmaps
        renderPixmap ptr offset lightOffset Nothing pixmap

littleLightOffsets :: ColorLights (Offset Double)
littleLightOffsets = ColorLights {
    red_ = Position redX lightsY,
    blue_ = Position blueX lightsY,
    green_ = Position greenX lightsY,
    yellow_ = Position yellowX lightsY
  }

redX, blueX, greenX, yellowX, lightsY :: Double
redX = lightsY
blueX = redX + boxWidth + padding
greenX = blueX + boxWidth + padding
yellowX = greenX + boxWidth + padding

lightsY = fromUber 15

boxWidth, padding :: Double
boxWidth = fromUber 3
padding = fromUber 2


-- * rendering of game OSD

renderTerminalOSD :: Ptr QPainter -> Seconds -> Scene Object_ -> IO ()
renderTerminalOSD ptr now scene@Scene{mode = mode@Base.TerminalMode{}} =
    let terminal = Base.terminal mode
        object = getMainlayerObject scene terminal
        sort = sort_ object
    in case (unwrapTerminalSort sort, unwrapTerminal object) of
        (Just sort, Just terminal) -> do
            clearScreen ptr $ modifyAlpha (const $ 0.6) black
            windowSize <- fmap fromIntegral <$> sizeQPainter ptr
            let pixmaps = osdPixmaps sort
                position = fmap fromIntegral $ osdPosition windowSize (osdBackground pixmaps)
                -- states of lights
                (colorStates, exitState) =
                    blinkenLightsState now (robots terminal) (state terminal)
            renderPixmap ptr zero position Nothing (osdBackground pixmaps)
            renderOsdCenters ptr position pixmaps colorStates
            renderOsdFrames ptr position pixmaps (state terminal) (selectedColorLights (robotIndex (state terminal)))
            renderOsdExit ptr position now pixmaps (state terminal) exitState
renderTerminalOSD _ _ _ = return ()

osdPosition :: Size Double -> Pixmap -> Qt.Position Int
osdPosition windowSize (pixmapSize -> pixSize) =
    fmap round (position -~ fmap (/ 2) (sizeToPosition pixSize))
  where
    position = Position (width windowSize * 0.5) (height windowSize * (1 - recip goldenRatio))

renderOsdCenters :: Ptr QPainter -> Qt.Position Double -> OsdPixmaps -> ColorLights Bool -> IO ()
renderOsdCenters ptr offset pixmaps states =
    mapM_ inner [red_, blue_, green_, yellow_]
  where
    inner :: (forall a . (ColorLights a -> a)) -> IO ()
    inner color = when (color states) $
        renderPixmap ptr offset (color osdCenterOffsets) Nothing (color (osdCenters pixmaps))

renderOsdFrames ptr offset pixmaps state selected =
    case (row state) of
        RobotRow -> mapM_ inner [red_, blue_, green_, yellow_]
        _ -> return ()
  where
    inner :: (forall a . (ColorLights a -> a)) -> IO ()
    inner color = when (color selected) $
        renderPixmap ptr offset (color osdFrameOffsets) Nothing (color (osdFrames pixmaps))

-- | offsets for frame pixmaps
osdFrameOffsets :: ColorLights (Qt.Position Double)
osdFrameOffsets =
    ColorLights red blue green yellow
  where
    red = fmap fromUber $ Position 5 5
    blue = toLeftFrame red
    green = toLeftFrame blue
    yellow = toLeftFrame green

    toLeftFrame = (+~ Position (fromUber 17) 0)

osdCenterOffsets :: ColorLights (Qt.Position Double)
osdCenterOffsets = fmap (+~ fmap fromUber (Position 2 2)) osdFrameOffsets

renderOsdExit ptr offset now pixmaps state exitState = do
    when (row state == NikkiRow) $
        renderPixmap ptr offset exitFrameOffset Nothing (osdExitFrame pixmaps)
    when exitState $
        renderPixmap ptr offset exitCenterOffset Nothing (osdExitCenter pixmaps)
  where
    exitFrameOffset = fmap fromUber $ Position 33 29
    exitCenterOffset = exitFrameOffset +~ fmap fromUber (Position 2 2)


-- * special edit mode (OEM)
-- how to attach robots to Terminals

oemMethods :: OEMMethods
oemMethods = OEMMethods
    (const $ OEMState NoRobots)
    (\ s -> OEMState (readNote "terminal OEM" s :: TerminalOEMState))


data TerminalOEMState
    = NoRobots
    | Robots {
        availableRobots :: [Index], -- INV: not null
        selectedRobot :: Index,
        attachedRobots :: [Index]
      }
  deriving (Read, Show, Typeable, Data)

instance IsOEMState TerminalOEMState where
    oemEnterMode = enterMode
    oemUpdate = editorUpdate
    oemRender = oemRender_
    oemPickle = show

enterMode :: Sort sort o => EditorScene sort -> TerminalOEMState -> TerminalOEMState
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

editorUpdate :: EditorScene sort -> AppButton -> TerminalOEMState -> TerminalOEMState
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

oemRender_ :: Sort sort o => Ptr QPainter -> EditorScene sort -> TerminalOEMState -> IO ()
oemRender_ ptr scene state = do
    offset <- transformation ptr (oemCursor scene state) (getCursorSize scene)
    renderObjectScene ptr offset scene
    renderOEMOSDs ptr offset scene state

oemCursor :: Sort sort o => EditorScene sort -> TerminalOEMState -> EditorPosition
oemCursor scene NoRobots = cursor scene
oemCursor scene (Robots available selected _) = editorPosition (getMainlayerEditorObject scene selected)

renderOEMOSDs :: Sort sort o => Ptr QPainter -> Offset Double -> EditorScene sort -> TerminalOEMState
    -> IO ()
renderOEMOSDs ptr offset scene NoRobots = return ()
renderOEMOSDs ptr offset scene (Robots _ selected attached) = do
    renderRobotBox (modifyAlpha (const 0.5) orange) (getMainlayerEditorObject scene selected)
    mapM_ (renderRobotBox (modifyAlpha (const 0.3) Qt.yellow)) $ map (getMainlayerEditorObject scene) $
        attached
  where
    renderRobotBox :: Sort sort o => Color -> EditorObject sort -> IO ()
    renderRobotBox color robot = do
        let sort = editorSort robot
            pos = editorPosition2QtPosition sort $ editorPosition robot
            size_ = size sort
        drawColoredBox ptr (pos +~ offset) size_ 4 color


-- * game logick

hasTerminalShape :: Terminal -> Shape -> Bool
hasTerminalShape terminal shape =
    shape `elem` shapes (chipmunk terminal)
