{-# language NamedFieldPuns, MultiParamTypeClasses, ScopedTypeVariables,
    ViewPatterns, DeriveDataTypeable, Rank2Types, FlexibleInstances,
    ImpredicativeTypes, RecordWildCards #-}

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
import Data.Accessor
import Data.Map (alter)

import Text.Logging

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt hiding (red, blue, green, yellow)
import qualified Graphics.Qt as Qt

import Utils

import Base hiding (Mode(..), update)
import qualified Base

import Sorts.Nikki.Configuration (nikkiSize)
import Sorts.LowerLimit (isBelowLowerLimit)

import Editor.Scene.Types
import Editor.Scene.Rendering
import Editor.Scene.Rendering.Helpers

import StoryMode.Types
import StoryMode.Episode


-- * terminal configuration

blinkenLightSpeed :: Seconds
blinkenLightSpeed = 0.5

blinkLength :: Seconds
blinkLength = 0.4

batteryTerminalSize = fmap fromUber $ Size 72 48

batteryNumberNeeded = 100

-- * sort loading

sorts :: RM [Sort_]
sorts = do
    a <- terminalSort
    mPngDir <- io $ getStoryModeDataFileName "png"
    sorts <- case mPngDir of
        Nothing -> return [a]
        Just pngDir -> do
            b <- batteryTerminalSort pngDir a
            return [a, b]
    return $ map Sort_ $ sorts

terminalSort :: RM TSort
terminalSort = do
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
    littleDefunctColors <- readColorLights (\ color -> toPngPath ("terminal-defunct-" ++ color))
    osdPixmaps <- loadOsdPixmaps
    return $ TSort
        (Pixmaps backgroundPixmap displayBlinkenLights littleColors littleDefunctColors)
        osdPixmaps

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
    defunctCenters <- fmapM (load 40) $ fmap (++ "-center-defunct") colors
    frames <- fmapM (load 38) $ fmap (++ "-frame") colors
    exitCenter <- (load 40) "exit-center"
    exitFrame <- (load 38) "exit-frame"
    return $ OsdPixmaps background centers defunctCenters frames exitCenter exitFrame
  where
    osdPath = pngDir </> "terminals" </> "osd"
    toOsdPath :: String -> RM FilePath
    toOsdPath name = getDataFileName (osdPath </> name <.> "png")
    -- removes the shadow to the right and bottom
    -- by decreasing the size
    removeUberPixelShadow :: Pixmap -> Pixmap
    removeUberPixelShadow p@Pixmap{pixmapSize} =
        p{pixmapSize = pixmapSize -~ fmap fromUber (Size 1 1)}

batteryTerminalSort :: FilePath -> TSort -> RM TSort
batteryTerminalSort pngDir tsort = do
    BatteryTSort tsort <$>
        loadPixmap (Position 5 5) batteryTerminalSize (mkPath "battery-terminal") <*>
        loadPix 9 "beam-white" <*>
        loadPix 9 "beam-green" <*>
        loadPix 9 "light-green" <*>
        fmapM (loadPix 9) bootingPix
  where
    loadPix n name =
        loadSymmetricPixmap (Position n n) (mkPath name)
    mkPath name = pngDir </> "battery-terminal" </> name <.> "png"
    bootingPix =
        "display_standby_00" :
        "display_booting_01" :
        "display_booting_02" :
        "display_booting_03" :
        "display_booting_04" :
        []

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
    littleColorLights :: ColorLights Pixmap,
    littleDefunctColorLights :: ColorLights Pixmap
  }
    deriving Show

freeTerminalPixmaps (Pixmaps a bs cs dcs) =
    freePixmap a >>
    fmapM_ freePixmap bs >>
    fmapM_ freePixmap cs >>
    fmapM_ freePixmap dcs

data OsdPixmaps = OsdPixmaps {
    osdBackground :: Pixmap,
    osdCenters :: ColorLights Pixmap,
    osdDefunctCenters :: ColorLights Pixmap,
    osdFrames :: ColorLights Pixmap,
    osdExitCenter :: Pixmap,
    osdExitFrame :: Pixmap
  }
    deriving (Show, Typeable)

freeOsdPixmaps (OsdPixmaps a bs cs ds e f) = do
    freePixmap a
    fmapM_ freePixmap bs
    fmapM_ freePixmap cs
    fmapM_ freePixmap ds
    freePixmap e
    freePixmap f


data TSort
  = TSort {
    pixmaps :: Pixmaps,
    osdPixmaps :: OsdPixmaps
  }
  | BatteryTSort {
    tsort :: TSort,
    btBackground :: Pixmap,
    whiteBeam :: Pixmap,
    greenBeam :: Pixmap,
    greenTop :: Pixmap,
    bootingPixmaps :: [Pixmap]
  }
    deriving (Show, Typeable)

getOsdPixmaps :: TSort -> OsdPixmaps
getOsdPixmaps TSort{..} = osdPixmaps
getOsdPixmaps BatteryTSort{..} = getOsdPixmaps tsort

isTerminal :: Sort sort o => sort -> Bool
isTerminal (cast -> Just _ :: Maybe TSort) = True
isTerminal (cast -> Just (Sort_ inner) :: Maybe Sort_) = isTerminal inner
isTerminal _ = False

data Terminal
  = Terminal {
    chipmunk :: Chipmunk,
    robots_ :: [RobotIndex],
    state_ :: State
  }
  | StandbyBatteryTerminal {
    chipmunk :: Chipmunk,
    robots_ :: [RobotIndex],
    batteryNumber :: Integer
  }
  | BatteryTerminal {
    chipmunk :: Chipmunk,
    robots_ :: [RobotIndex],
    state_ :: State,
    onTime :: Seconds
  }
    deriving (Show, Typeable)

state :: Accessor Terminal State
state = accessor state_ (\ a r -> r{state_ = a})

robots :: Accessor Terminal [RobotIndex]
robots = accessor robots_ (\ a r -> r{robots_ = a})

unwrapTerminal :: Object_ -> Maybe Terminal
unwrapTerminal (Object_ sort o) = cast o

unwrapTerminalSort :: Sort_ -> Maybe TSort
unwrapTerminalSort (Sort_ sort) = cast sort

terminalExitMode :: Terminal -> ExitMode
terminalExitMode Terminal{..} = exitMode state_
terminalExitMode BatteryTerminal{..} = exitMode state_
terminalExitMode StandbyBatteryTerminal{} = ExitToNikki -- exit to nikki immediately


data RobotIndex
    = Controllable {unwrapRobotIndex :: Index}
    | Uncontrollable {unwrapRobotIndex :: Index}
  deriving (Show, Typeable)


data State
    = State {
        gameMode_ :: GameMode,
        row :: MenuState,
        robotIndex :: Int,
        changedTime :: Seconds,
        exitMode :: ExitMode
      }
  deriving Show

gameMode :: Accessor State GameMode
gameMode = accessor gameMode_ (\ a r -> r{gameMode_ = a})

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
    | Defunct -- when robots are uncontrollable

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
        Defunct
    zipRobotsSelected Nothing _ =
        Off

    mapRobots :: Maybe RobotIndex -> LightState
    mapRobots (Just (Controllable _)) = On
    mapRobots (Just (Uncontrollable _)) = Defunct
    mapRobots Nothing = Off

    blinkingOut = blinkingMode && even (floor ((now - changedTime state) / blinkLength))
    blinkingMode = case state ^. gameMode of
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
    sortId TSort{} = SortId "terminal"
    sortId BatteryTSort{} = SortId "story-mode/batteryTerminal"
    freeSort (TSort terminalPixmaps osdPixmaps) =
        freeTerminalPixmaps terminalPixmaps >>
        freeOsdPixmaps osdPixmaps
    freeSort (BatteryTSort _ a b c d e) =
        fmapM_ freePixmap (a : b : c : d : e)
    size TSort{} = fmap fromUber $ Size 48 48
    size BatteryTSort{} = batteryTerminalSize
    renderIconified sort@TSort{..} ptr =
        renderPixmapSimple ptr $ background pixmaps
    renderIconified sort@BatteryTSort{btBackground} ptr =
        renderPixmapSimple ptr btBackground

    objectEditMode _ = Just oemMethods

    initialize app file (Just space) sort editorPosition (Just (OEMState oemState_)) _ = io $ do
        let Just oemState :: Maybe TerminalOEMState = cast oemState_
            attached = fmap Controllable $ case oemState of
                NoRobots -> []
                Robots _ _ x -> x
            pos = position2vector
                (epToPosition (size sort) editorPosition)
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
        case sort of
            TSort{} -> return $ Terminal chip attached (initialMenuState 0)
            BatteryTSort{} -> mkBatteryTerminal file chip attached
    initialize app _ Nothing sort@TSort{} editorPosition _ _ = do
        let position = epToPosition (size sort) editorPosition
            (_, baryCenterOffset) = mkPolys $ size sort
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        return $ case sort of
            TSort{} -> Terminal chip [] (initialMenuState 0)
            BatteryTSort{} -> StandbyBatteryTerminal chip [] 0

    immutableCopy t =
        CM.immutableCopy (chipmunk t) >>= \ x -> return t{chipmunk = x}

    getControlledChipmunk scene _ =
        getControlledChipmunk scene $
        scene ^. mainLayerObjectA (Base.nikki $ scene ^. mode)

    chipmunks = chipmunk >>> return

    startControl now t@StandbyBatteryTerminal{} = t
    startControl now t =
        state ^= reset (now - blinkLength) (t ^. robots) (t ^. state) $
        t

    updateNoSceneChange = update

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

update sort config _ scene now contacts (False, cd) terminal@StandbyBatteryTerminal{} = do
    logg Debug $ show $ batteryNumber terminal
    logg Debug $ take 10 $ show terminal
    return terminal
update sort config _ scene now contacts (True, cd) terminal@StandbyBatteryTerminal{} = do
    logg Debug $ show $ batteryNumber terminal
    logg Debug $ take 10 $ show terminal
    updateStandbyState now <$>
        putBatteriesInTerminal scene now terminal
update sort config _ scene now contacts (False, cd) terminal = do
    logg Debug $ take 10 $ show terminal
    (state ^: updateGameMode contacts terminal) <$>
        (robots ^^: updateControllableStates scene) terminal
update sort config _ scene now contacts (True, cd) terminal = do
    logg Debug $ take 10 $ show terminal
    (robots ^^: updateControllableStates scene) $
        state ^= updateState config now cd (terminal ^. robots) (terminal ^. state) $
        terminal


-- | updates the gameMode if Nikki doesn't touch it anymore
updateGameMode :: Contacts -> Terminal -> State -> State
updateGameMode contacts terminal state =
    if fany (hasTerminalShape terminal) (terminals contacts)
    -- Nikki still touches the terminal
    then state
    -- Nikki doesn't touch the terminal, so back to NikkiMode
    else
        gameMode ^= NikkiMode $
        state

-- | updates the controllable states of the attached robots
updateControllableStates :: Scene Object_ -> [RobotIndex] -> IO [RobotIndex]
updateControllableStates scene =
    fmapM (updateControllableState scene)

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

-- | initializes battery terminals
mkBatteryTerminal :: LevelFile -> Chipmunk -> [RobotIndex] -> IO Terminal
mkBatteryTerminal file chip robots =
    updateStandbyState 0 <$>
    case file of
        (EpisodeLevel episode _ _ _) -> do
            batteries <- batteriesInTerminal <$> getEpisodeScore (euid episode)
            return $ StandbyBatteryTerminal chip robots batteries
        -- not in story-mode (shouldn't happen at all, just for testing)
        _ -> return $ StandbyBatteryTerminal chip robots 0

putBatteriesInTerminal :: Scene o -> Seconds -> Terminal -> IO Terminal
putBatteriesInTerminal scene now t@StandbyBatteryTerminal{} =
    case levelFile scene of
        (EpisodeLevel episode _ _ _) -> do
            score <- getEpisodeScore $ euid episode
            batteries <- getCollectedBatteries scene episode
            setEpisodeScore (euid episode) (EpisodeScore_0 True batteries)
            return $ StandbyBatteryTerminal
                (chipmunk t)
                (t ^. robots)
                batteries
        _ -> return t

getCollectedBatteries scene episode =
    getHighScores >$> \ hs ->
        -- insert the actual collected batteries for the current level
    let file = levelFile scene
        upToDateScore = alter alteration (levelUID file) $ fmap (^. scoreBatteryPower) hs
        alteration :: Maybe Integer -> Maybe Integer
        alteration Nothing = Just $ (scene ^. batteryPower)
        alteration (Just s) = Just $ max (scene ^. batteryPower) s
    -- TODO: pretend, there are no batteries in the outro level for now
    in collectedBatteries episode $ fmap (^. scoreBatteryPower) hs -- upToDateScore

updateStandbyState :: Seconds -> Terminal -> Terminal
updateStandbyState now t@StandbyBatteryTerminal{..} =
    if batteryNumber < batteryNumberNeeded then t else
        BatteryTerminal chipmunk robots_ (initialMenuState now) now


-- * game rendering

renderTerminal :: TSort -> Seconds
    -> Terminal -> Qt.Position Double
    -> [RenderPixmap]
renderTerminal sort@TSort{} now t pos =
    renderTerminalBackground sort pos :
    renderDisplayBlinkenLights sort now pos :
    catMaybes (renderLittleColorLights sort now t pos)
renderTerminal sort@BatteryTSort{..} now t pos =
    let background = RenderPixmap btBackground pos Nothing
        blinkenLights = renderDisplayBlinkenLights tsort now pos
        batteryBar = renderBatteryBar sort pos
        booting = renderBootingAnimation sort pos
    in  background :
--         blinkenLights :
        batteryBar :
        booting :
        []

renderTerminalBackground sort pos =
    RenderPixmap (background $ pixmaps sort) pos Nothing

-- | renders the main terminal pixmap (with blinkenlights)
renderDisplayBlinkenLights sort now pos =
    let pixmap = pickAnimationFrame (blinkenLights $ pixmaps sort)
                 [blinkenLightSpeed] now
    in RenderPixmap pixmap (pos +~ blinkenLightOffset) Nothing

blinkenLightOffset = fmap fromUber $ Position 13 18

-- | renders the little colored lights (for the associated robots) on the terminal in the scene
renderLittleColorLights sort now t pos =
    let colorStates = fst $ blinkenLightsState now (t ^. robots) (t ^. state)
    in map
        (renderLight
            (littleColorLights $ pixmaps sort)
            (littleDefunctColorLights $ pixmaps sort)
            pos colorStates)
        allSelectors

renderLight :: ColorLights Pixmap -> ColorLights Pixmap
    -> Qt.Position Double -> ColorLights LightState
    -> (forall a . (ColorLights a -> a))
    -> Maybe RenderPixmap
renderLight pixmaps defunctPixmaps pos colorStates color =
    let lightOffset = color littleLightOffsets
        pixmap = color pixmaps
        defunctPixmap = color defunctPixmaps
        render p = Just $ RenderPixmap p (pos +~ lightOffset) Nothing
    in case color colorStates of
        On -> render pixmap
        Off -> Nothing
        Defunct -> render defunctPixmap

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


-- * battery terminal rendering

renderBatteryBar BatteryTSort{..} p =
    RenderPixmap whiteBeam (p +~ whiteBeamOffset) Nothing

whiteBeamOffset = fmap fromUber $ Position 55 32

renderBootingAnimation BatteryTSort{..} p =
    RenderPixmap (head bootingPixmaps) (p +~ bootingOffset) Nothing

bootingOffset = fmap fromUber $ Position 30 20


-- * rendering of game OSD

renderTerminalOSD :: Ptr QPainter -> Seconds -> Scene Object_ -> IO ()
renderTerminalOSD ptr now scene@Scene{mode_ = mode@Base.TerminalMode{}} =
    let terminal = Base.terminal mode
        object = scene ^. mainLayerObjectA terminal
        sort = sort_ object
    in case (unwrapTerminalSort sort, unwrapTerminal object) of
        (Just sort, Just terminal@StandbyBatteryTerminal{}) ->
            return ()
        (Just sort, Just terminal) -> do
            clearScreen ptr $ alpha ^= 0.6 $ black
            windowSize <- sizeQPainter ptr
            let pixmaps = getOsdPixmaps sort
                position = fmap fromIntegral $ osdPosition windowSize (osdBackground pixmaps)
                -- states of lights
                (colorStates, exitState) =
                    blinkenLightsState now (terminal ^. robots) (terminal ^. state)
            renderPixmap ptr zero position Nothing (osdBackground pixmaps)
            renderOsdCenters ptr position pixmaps colorStates
            renderOsdFrames ptr position pixmaps (terminal ^. state) (selectedColorLights (robotIndex (terminal ^. state)))
            renderOsdExit ptr position now pixmaps (terminal ^. state) exitState
renderTerminalOSD _ _ _ = return ()

osdPosition :: Size Double -> Pixmap -> Qt.Position Int
osdPosition windowSize (pixmapSize -> pixSize) =
    fmap round (position -~ fmap (/ 2) (size2position pixSize))
  where
    position = Position (width windowSize * 0.5) (height windowSize * (1 - recip goldenRatio))

renderOsdCenters :: Ptr QPainter -> Qt.Position Double -> OsdPixmaps -> ColorLights LightState -> IO ()
renderOsdCenters ptr offset pixmaps states =
    mapM_ inner allSelectors
  where
    inner :: (forall a . (ColorLights a -> a)) -> IO ()
    inner color = case (color states) of
        On -> renderCenter color osdCenters
        Off -> return ()
        Defunct -> renderCenter color osdDefunctCenters
    renderCenter (color :: forall a . (ColorLights a -> a)) pixmapsGetter =
        renderPixmap ptr offset
            (color osdCenterOffsets) Nothing (color (pixmapsGetter pixmaps))

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

editorUpdate :: EditorScene sort -> Button -> TerminalOEMState
    -> OEMUpdateMonad TerminalOEMState
editorUpdate _ (KeyboardButton key _) NoRobots | key `elem` keys = return NoRobots
  where
    keys = (RightArrow : LeftArrow : Return : Enter : [])
editorUpdate _ _ NoRobots = oemNothing
editorUpdate scene (KeyboardButton key _) state@(Robots available selected attached) =
  case key of
    RightArrow -> return state{selectedRobot = searchNext selected available}
    LeftArrow -> return state{selectedRobot = searchNext selected (reverse available)}
    x | (x == Return || x == Enter || x == Ctrl) ->
        if length attached < 4 then
            return state{attachedRobots = swapIsElem selected attached}
          else
            oemError
    _ -> oemNothing
editorUpdate _ _ _ = oemNothing

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
            pos = epToPosition size_ $ robot ^. editorPosition
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
