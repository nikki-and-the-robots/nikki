{-# language NamedFieldPuns, MultiParamTypeClasses, ScopedTypeVariables,
     ViewPatterns, DeriveDataTypeable, Rank2Types, FlexibleInstances, ImpredicativeTypes #-}

{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}

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
import Data.Maybe

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt hiding (red, blue, green, yellow)
import qualified Graphics.Qt as Qt

import Utils

import Base hiding (Mode(..))
import qualified Base

import Sorts.Nikki.Configuration (nikkiSize)
import Sorts.LowerLimit (isBelowLowerLimit)

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
    let nameToPixmap =
            return . toPngPath >=>
            getDataFileName
        backgroundOffset = Position (1 + fromUber 1) (1 + fromUber 1)
        backgroundSize = fmap fromUber $ Size 48 48
    backgroundPixmap <- loadPixmap backgroundOffset backgroundSize =<<
        nameToPixmap "terminal-standard"
    displayBlinkenLights <- fmapM (nameToPixmap >=> loadSymmetricPixmap (Position 1 1)) (
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
    fmapM (return . f >=> getDataFileName >=> loadSymmetricPixmap (Position 13 13)) $
        ColorLights "red" "blue" "green" "yellow"

loadOsdPixmaps :: RM OsdPixmaps
loadOsdPixmaps = do
    background <- removeUberPixelShadow <$>
                    (loadSymmetricPixmap (Position 1 1) =<< toOsdPath "background")
    let colors = ColorLights "red" "blue" "green" "yellow"
        load :: Double ->  String -> RM Pixmap
        load offset = toOsdPath >=> loadSymmetricPixmap (Position offset offset)
    centers <- fmapM (load 40) $ fmap (++ "-center") colors
    frames <- fmapM (load 38) $ fmap (++ "-frame") colors
    exitCenter <- (load 40) "exit-center"
    exitFrame <- (load 38) "exit-frame"
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

allSelectors :: [forall a . (ColorLights a -> a)]
allSelectors = [red_, blue_, green_, yellow_]

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

freeTerminalPixmaps (Pixmaps a bs cs) =
    freePixmap a >>
    fmapM_ freePixmap bs >>
    fmapM_ freePixmap cs

data OsdPixmaps = OsdPixmaps {
    osdBackground :: Pixmap,
    osdCenters :: ColorLights Pixmap,
    osdFrames :: ColorLights Pixmap,
    osdExitCenter :: Pixmap,
    osdExitFrame :: Pixmap
  }
    deriving (Show, Typeable)

freeOsdPixmaps (OsdPixmaps a bs cs d e) = do
    freePixmap a
    fmapM_ freePixmap bs
    fmapM_ freePixmap cs
    freePixmap d
    freePixmap e


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
    robots :: [RobotIndex],
    state :: State
  }
    deriving (Show, Typeable)

unwrapTerminal :: Object_ -> Maybe Terminal
unwrapTerminal (Object_ sort o) = cast o

unwrapTerminalSort :: Sort_ -> Maybe TSort
unwrapTerminalSort (Sort_ sort) = cast sort

terminalExitMode :: Terminal -> ExitMode
terminalExitMode = state >>> exitMode


data RobotIndex
    = Controllable {unwrapRobotIndex :: Index}
    | Uncontrollable {unwrapRobotIndex :: Index}
  deriving (Show, Typeable)


data State
    = State {
        gameMode :: GameMode,
        row :: MenuState,
        robotIndex :: Int,
        changedTime :: Seconds,
        exitMode :: ExitMode
      }
  deriving Show

data MenuState = NikkiState | RobotState
  deriving (Eq, Show)

initialMenuState :: Seconds -> State
initialMenuState now = State NikkiMode RobotState 0 now DontExit

isNikkiSelected :: State -> Bool
isNikkiSelected (State _ NikkiState _ _ _) = True
isNikkiSelected (State _ RobotState _ _ _) = False

-- | resets the terminal state, when it is started to be used.
reset :: Seconds -> [RobotIndex] -> State -> State
reset t robots (State _ _ i _ _) =
    State TerminalMode row i t DontExit
  where
    row = if null robots then NikkiState else RobotState

data LightState
    = On
    | Off
    | Disabled -- when robots are uncontrollable

-- | returns robot states and exit (aka nikki or eject) state
blinkenLightsState :: Seconds -> [RobotIndex] -> State -> (ColorLights LightState, Bool)
blinkenLightsState now robots state =
    case row state of
        NikkiState -> (fmap mapRobots robotIndices, not blinkingOut)
        RobotState -> tuple
            (fzipWith zipRobotsSelected robotIndices selected)
            True
  where
    selected = selectedColorLights i
    i = robotIndex state

    robotIndices :: ColorLights (Maybe RobotIndex)
    robotIndices = fmap (\ i -> if i < length robots then Just (robots !! i) else Nothing) $ ColorLights 0 1 2 3

    zipRobotsSelected :: Maybe RobotIndex -> Bool -> LightState
    zipRobotsSelected (Just (Controllable _)) selected =
        if not selected || not blinkingOut then On else Off
    zipRobotsSelected (Just (Uncontrollable _)) selected =
        Disabled
    zipRobotsSelected Nothing _ =
        Off

    mapRobots :: Maybe RobotIndex -> LightState
    mapRobots (Just (Controllable _)) = On
    mapRobots (Just (Uncontrollable _)) = Disabled
    mapRobots Nothing = Off

    blinkingOut = blinkingMode && even (floor ((now - changedTime state) / blinkLength))
    blinkingMode = case gameMode state of
        NikkiMode -> False
        _ -> True

-- | changes the selection of a robot (if applicable)
-- and updates the selectedChangedTime (also if applicable)
modifySelected :: Seconds -> [RobotIndex] -> (Int -> Int) -> State -> State
modifySelected now robots f state =
    case row state of
        NikkiState -> state
        RobotState -> if newIndex /= robotIndex state
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
    freeSort (TSort terminalPixmaps osdPixmaps) =
        freeTerminalPixmaps terminalPixmaps >>
        freeOsdPixmaps osdPixmaps
    size = const $ Size (fromUber 48) (fromUber 48)
    renderIconified sort ptr =
        renderPixmapSimple ptr $ background $ pixmaps sort

    objectEditMode _ = Just oemMethods

    initialize app (Just space) sort editorPosition (Just (OEMState oemState_)) = io $ do
        let Just oemState :: Maybe TerminalOEMState = cast oemState_
            attached = fmap Controllable $ case oemState of
                NoRobots -> []
                Robots _ _ x -> x
            pos = position2vector
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
    initialize app Nothing sort editorPosition _ = do
        let position = editorPosition2QtPosition sort editorPosition
            (_, baryCenterOffset) = mkPolys $ size sort
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        return $ Terminal chip [] (initialMenuState 0)

    immutableCopy t =
        CM.immutableCopy (chipmunk t) >>= \ x -> return t{chipmunk = x}

    getControlledChipmunk scene _ =
        getControlledChipmunk scene $
        scene ^. mainLayerObjectA (Base.nikki $ scene ^. mode)

    chipmunks = chipmunk >>> return

    startControl now t = t{state = reset (now - blinkLength) (robots t) (state t)}

    updateNoSceneChange sort config scene now contacts (False, cd) terminal =
        updateControllableStates scene terminal
    updateNoSceneChange sort config scene now contacts (True, cd) terminal =
        updateControllableStates scene
            terminal{state = updateState config now cd (robots terminal) (state terminal)}

    renderObject _ _ terminal sort ptr offset now = do
        pos <- fst <$> getRenderPositionAndAngle (chipmunk terminal)
        return $ renderTerminal sort now terminal pos


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
    (Size wh hh) :: Size CpFloat = fmap realToFrac $ fmap (/ 2) size
    baryCenterOffset = Vector wh hh
    -- the physics object is much narrower to simulate,
    -- nikki being able to use the terminal if nikki is completely
    -- inside the terminal frame. (With height, there is no such padding,
    -- as nikki can only jump when standing on the ground.)
    padding = realToFrac $ width nikkiSize


-- * controlling

-- | updates the controllable states of the attached robots
updateControllableStates :: Scene Object_ -> Terminal -> IO Terminal
updateControllableStates scene terminal = do
    robots' <- fmapM (updateControllableState scene) $ robots terminal
    return terminal{robots = robots'}

updateControllableState :: Scene Object_ -> RobotIndex -> IO RobotIndex
updateControllableState scene (unwrapRobotIndex -> i) = do
    robotPosition <- getPosition $ getControlledChipmunk scene (scene ^. mainLayerObjectA i)
    let cons = if isBelowLowerLimit scene robotPosition then Uncontrollable else Controllable
    return $ cons i

-- | controls the terminal in terminal mode
updateState :: Controls -> Seconds -> ControlData -> [RobotIndex] -> State -> State
updateState config now cd robots state | isTerminalConfirmationPressed config cd =
  case row state of
    NikkiState -> exitToNikki state
    RobotState -> case (robots !! robotIndex state) of
        Controllable i -> state{exitMode = ExitToRobot i}
        Uncontrollable i -> state -- TODO: sound
updateState config now cd robots state@State{row = RobotState}
    | (isGameRightPressed config cd) && not (null robots) =
        -- go right in robot list
        modifySelected now robots (+ 1) state

updateState config now cd robots state@State{row = RobotState} | isGameLeftPressed config cd =
    if robotIndex state > 0 then
        -- go left in robot list
        modifySelected now robots (subtract 1) state
      else
        -- select exit (nikki) menu item
        state{row = NikkiState, changedTime = now}
updateState config now cd robots state@State{row = NikkiState}
    | isGameRightPressed config cd && not (null robots) =
        -- go to robot list
        state{row = RobotState, changedTime = now}
updateState _ _ _ _ t = t

exitToNikki :: State -> State
exitToNikki state = state{exitMode = ExitToNikki, robotIndex = 0}


-- * game rendering

renderTerminal :: TSort -> Seconds
    -> Terminal -> Qt.Position Double
    -> [RenderPixmap]
renderTerminal sort now t pos =
    renderTerminalBackground sort pos :
    renderDisplayBlinkenLights sort now pos :
    catMaybes (renderLittleColorLights sort now t pos)

renderTerminalBackground sort pos =
    RenderPixmap (background $ pixmaps sort) pos Nothing

-- | renders the main terminal pixmap (with blinkenlights)
renderDisplayBlinkenLights sort now pos =
    let pixmap = pickAnimationFrame (blinkenLights $ pixmaps sort)
                 [blinkenLightSpeed] now
    in RenderPixmap pixmap (pos +~ blinkenLightOffset) Nothing

blinkenLightOffset = fmap fromUber $ Position 13 13

-- | renders the little colored lights (for the associated robots) on the terminal in the scene
renderLittleColorLights sort now t pos =
    let colorStates = fst $ blinkenLightsState now (robots t) (state t)
    in map
        (renderLight (littleColorLights $ pixmaps sort) pos colorStates)
        allSelectors

renderLight :: ColorLights Pixmap -> Qt.Position Double -> ColorLights LightState
    -> (forall a . (ColorLights a -> a))
    -> Maybe RenderPixmap
renderLight pixmaps pos colorStates color =
    let lightOffset = color littleLightOffsets
        pixmap = color pixmaps
    in case color colorStates of
        On -> Just $ RenderPixmap pixmap (pos +~ lightOffset) Nothing
        Off -> Nothing
        Disabled -> Just $ RenderCommand (pos +~ lightOffset) $ \ ptr -> do
            setPenColor ptr (alpha ^= 0.5 $ pink) 4
            drawLine ptr zero (sizeToPosition $ pixmapSize pixmap)

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
renderTerminalOSD ptr now scene@Scene{mode_ = mode@Base.TerminalMode{}} =
    let terminal = Base.terminal mode
        object = scene ^. mainLayerObjectA terminal
        sort = sort_ object
    in case (unwrapTerminalSort sort, unwrapTerminal object) of
        (Just sort, Just terminal) -> do
            clearScreen ptr $ alpha ^= 0.6 $ black
            windowSize <- sizeQPainter ptr
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

renderOsdCenters :: Ptr QPainter -> Qt.Position Double -> OsdPixmaps -> ColorLights LightState -> IO ()
renderOsdCenters ptr offset pixmaps states =
    mapM_ inner allSelectors
  where
    inner :: (forall a . (ColorLights a -> a)) -> IO ()
    inner color = case (color states) of
        On -> renderPixmap ptr offset (color osdCenterOffsets) Nothing (color (osdCenters pixmaps))
        Off -> return ()
        Disabled -> do
            resetMatrix ptr
            translate ptr (offset +~ (color osdCenterOffsets))
            setPenColor ptr (alpha ^= 0.5 $ pink) 4
            let pixmap = color (osdCenters pixmaps)
            drawLine ptr zero (sizeToPosition $ pixmapSize pixmap)

renderOsdFrames ptr offset pixmaps state selected =
    case (row state) of
        RobotState -> mapM_ inner allSelectors
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
    red = fmap fromUber $ Position 31 5
    blue = toLeftFrame red
    green = toLeftFrame blue
    yellow = toLeftFrame green

    toLeftFrame = (+~ Position (fromUber 20) 0)

osdCenterOffsets :: ColorLights (Qt.Position Double)
osdCenterOffsets = fmap (+~ fmap fromUber (Position 2 2)) osdFrameOffsets

renderOsdExit ptr offset now pixmaps state exitState = do
    when (row state == NikkiState) $
        renderPixmap ptr offset exitFrameOffset Nothing (osdExitFrame pixmaps)
    when exitState $
        renderPixmap ptr offset exitCenterOffset Nothing (osdExitCenter pixmaps)
  where
    exitFrameOffset = fmap fromUber $ Position 5 5
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
    oemNormalize = removeDeletedRobots
    oemRender ptr _ _ = oemRender_ ptr
    oemPickle = show
    oemHelp = const oemHelpText

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

editorUpdate :: EditorScene sort -> Button -> TerminalOEMState -> Maybe TerminalOEMState
editorUpdate _ (KeyboardButton key _) NoRobots | key `elem` keys = Just NoRobots
  where
    keys = (RightArrow : LeftArrow : Return : Enter : [])
editorUpdate _ _ NoRobots = Nothing
editorUpdate scene (KeyboardButton key _) state@(Robots available selected attached) =
  case key of
    RightArrow -> Just state{selectedRobot = searchNext selected available}
    LeftArrow -> Just state{selectedRobot = searchNext selected (reverse available)}
    x | (x == Return || x == Enter || x == Ctrl) -> Just state{attachedRobots = swapIsElem selected attached}
    _ -> Nothing
editorUpdate _ _ _ = Nothing

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

-- | removes the attached robots from the terminal, that have been deleted
removeDeletedRobots :: Sort sort o => EditorScene sort
    -> TerminalOEMState -> TerminalOEMState
removeDeletedRobots scene NoRobots = NoRobots
removeDeletedRobots scene (Robots a b attached) =
    Robots a b (filter (`elem` getRobotIndices scene) attached)


-- * rendering of OEM

oemRender_ :: Sort sort o => Ptr QPainter -> EditorScene sort -> TerminalOEMState -> IO ()
oemRender_ ptr scene state = do
    offset <- transformation ptr (oemCursor scene state) (getCursorSize scene)
    renderObjectScene ptr offset scene
    renderOEMOSDs ptr offset scene state

oemCursor :: Sort sort o => EditorScene sort -> TerminalOEMState -> EditorPosition
oemCursor scene NoRobots = cursor scene
oemCursor scene (Robots available selected _) =
    getMainLayerEditorObject scene selected ^. editorPosition

renderOEMOSDs :: Sort sort o => Ptr QPainter -> Offset Double -> EditorScene sort -> TerminalOEMState
    -> IO ()
renderOEMOSDs ptr offset scene NoRobots = return ()
renderOEMOSDs ptr offset scene (Robots _ selected attached) = do
    renderRobotBox (alpha ^= 0.5 $ orange) (getMainLayerEditorObject scene selected)
    mapM_ (renderRobotBox (alpha ^= 0.3 $ Qt.yellow)) $ map (getMainLayerEditorObject scene) $
        attached
  where
    renderRobotBox :: Sort sort o => Color -> EditorObject sort -> IO ()
    renderRobotBox color robot = do
        let sort = editorSort robot
            pos = editorPosition2QtPosition sort $ robot ^. editorPosition
            size_ = size sort
        drawColoredBox ptr (pos +~ offset) size_ 4 color


-- * game logick

hasTerminalShape :: Terminal -> Shape -> Bool
hasTerminalShape terminal shape =
    shape `elem` shapes (chipmunk terminal)

-- * help text

oemHelpText :: String
oemHelpText =
    "Right, Left: cycle through robots\n" ++
    "Ctrl: attach robot to terminal\n" ++
    "Shift: remove robot from terminal"
