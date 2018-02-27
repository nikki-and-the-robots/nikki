{-# language ViewPatterns, Rank2Types, ImpredicativeTypes, RecordWildCards,
    ScopedTypeVariables, NamedFieldPuns, MultiParamTypeClasses,
    DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

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

    roundToBars, -- for testing
  ) where

import           Data.Abelian
import           Data.Accessor
import           Data.Data
import           Data.Indexable (Index)
import           Data.Maybe
import qualified Data.StrictList as SL
import           Safe
import           System.FilePath

import qualified Base
import           Base hiding (Mode(..), update)
import           Editor.Scene.Rendering
import           Editor.Scene.Rendering.Helpers
import           Editor.Scene.Types
import qualified Graphics.Qt as Qt
import           Graphics.Qt hiding (red, blue, green, yellow)
import           Physics.Chipmunk as CM
import           Sorts.LowerLimit (isBelowLowerLimit)
import           Sorts.Nikki.Configuration (nikkiSize)
import           Sorts.Sign (renderSpeechBubble, bubbleTextWidths)
import           StoryMode.Configuration
import           StoryMode.Episode
import           StoryMode.Types
import           Utils

-- * terminal configuration

blinkenLightSpeed :: Seconds
blinkenLightSpeed = 0.5

blinkLength :: Seconds
blinkLength = 0.4

standardTerminalSize = fmap fromUber $ Size 48 48
batteryTerminalSize = fmap fromUber $ Size 72 48

beamBlinkingTime :: Seconds = blinkLength

bootingFrameTime :: Seconds = 0.3
bootingAnimationSteps :: Int = 3

-- * sort loading

sorts :: [RM (Maybe Sort_)]
sorts =
    -- normal
    (Just <$> Sort_ <$> terminalSort False) :
    -- transparent
    (Just <$> Sort_ <$> terminalSort True) :
    (fmap Sort_ <$> mkBatteryTerminalSort) :
    []

terminalSort :: Bool -> RM TSort
terminalSort transparent = do
    let nameToPixmap =
            return . toPngPath >=>
            getDataFileName
        backgroundOffset = Position (1 + fromUber 1) (1 + fromUber 1)
        backgroundSize = fmap fromUber $ Size 48 48
    backgroundPixmap <- loadPixmap backgroundOffset backgroundSize =<<
        nameToPixmap "terminal-standard"
    colorBar <- loadSymmetricPixmap (Position 9 9) =<< nameToPixmap "display-colors"
    displayBlinkenLights <- fmapM (nameToPixmap >=> loadSymmetricPixmap (Position 1 1)) (
        "display_00" :
        "display_01" :
        "display_02" :
        "display_03" :
      [])
    let blinkenLightsAnimation = mkAnimation displayBlinkenLights [blinkenLightSpeed]
    littleColors <- readColorLights (\ color -> toPngPath ("terminal-" ++ color))
    littleDefunctColors <- readColorLights (\ color -> toPngPath ("terminal-defunct-" ++ color))
    osdPixmaps <- loadOsdPixmaps
    return $ TSort
            (Pixmaps backgroundPixmap blinkenLightsAnimation
                     colorBar littleColors littleDefunctColors)
            osdPixmaps transparent

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


mkBatteryTerminalSort :: RM (Maybe TSort)
mkBatteryTerminalSort = do
    mPngDir <- io $ getStoryModeDataFileName "png"
    tsort <- terminalSort False
    case mPngDir of
        Nothing -> return Nothing
        Just pngDir -> do
            b <- batteryTerminalSort pngDir tsort
            return $ Just b

batteryTerminalSort :: FilePath -> TSort -> RM TSort
batteryTerminalSort pngDir tsort = do
    BatteryTSort tsort <$>
        loadPix 9 "beam-white" <*>
        loadPix 9 "beam-green" <*>
        loadPix 9 "light-green" <*>
        loadPix 9 "display_standby_00" <*>
        (mkAnimation <$> fmapM (loadPix 9) bootingPix <*> pure [bootingFrameTime]) <*>
        loadSound ("game" </> "batteryTerminalInsert") 1 <*>
        loadSound ("game" </> "batteryTerminalBooting") 1
  where
    loadPix n name =
        loadSymmetricPixmap (Position n n) (mkPath name)
    mkPath name = pngDir </> "battery-terminal" </> name <.> "png"
    bootingPix =
        "display_booting_01" :
        "display_booting_02" :
        "display_booting_03" :
        []

-- | type to bundle things for the four terminal colors: red, blue, green and yellow (in that order)
data ColorLights a = ColorLights {
    red_, blue_, green_, yellow_ :: a
  }
    deriving (Show, Functor, Foldable, Traversable)

allSelectors :: [forall a . (ColorLights a -> a)]
allSelectors = [red_, blue_, green_, yellow_]

fzipWith :: (a -> b -> c) -> ColorLights a -> ColorLights b -> ColorLights c
fzipWith f (ColorLights a b c d) (ColorLights p q r s) =
    ColorLights (f a p) (f b q) (f c r) (f d s)

selectedColorLights :: Int -> ColorLights Bool
selectedColorLights i = ColorLights (i == 0) (i == 1) (i == 2) (i == 3)


data Pixmaps = Pixmaps {
    background :: Pixmap,
    blinkenLights :: Animation Pixmap,
    colorBar :: Pixmap,
    littleColorLights :: ColorLights Pixmap,
    littleDefunctColorLights :: ColorLights Pixmap
  }
    deriving Show

data OsdPixmaps = OsdPixmaps {
    osdBackground :: Pixmap,
    osdCenters :: ColorLights Pixmap,
    osdDefunctCenters :: ColorLights Pixmap,
    osdFrames :: ColorLights Pixmap,
    osdExitCenter :: Pixmap,
    osdExitFrame :: Pixmap
  }
    deriving (Show, Typeable)


data TSort
  = TSort {
    pixmaps_ :: Pixmaps,
    osdPixmaps :: OsdPixmaps,
    transparent :: Bool -- transparent terminals for the story mode
  }
  | BatteryTSort {
    tsort :: TSort,
    whiteBeam :: Pixmap,
    greenBeam :: Pixmap,
    greenTop :: Pixmap,
    bootingStandBy :: Pixmap,
    bootingPixmaps :: Animation Pixmap,
    insertionSound :: PolySound,
    bootingSound :: PolySound
  }
    deriving (Show, Typeable)

pixmaps :: TSort -> Pixmaps
pixmaps TSort{pixmaps_} = pixmaps_
pixmaps BatteryTSort{tsort} = pixmaps tsort

getOsdPixmaps :: TSort -> OsdPixmaps
getOsdPixmaps TSort{..} = osdPixmaps
getOsdPixmaps BatteryTSort{..} = getOsdPixmaps tsort

data Terminal
  = Terminal {
    chipmunk :: Chipmunk,
    robots_ :: [RobotIndex],
    state_ :: State
  }
  | StandbyBatteryTerminal { -- battery terminal without enough batteries
    chipmunk :: Chipmunk,
    robots_ :: [RobotIndex],
    batteryNumber_ :: Integer,
    showingBubble_ :: Bool,
    onTime :: Maybe Seconds
  }
  | BatteryTerminal {
    chipmunk :: Chipmunk,
    robots_ :: [RobotIndex],
    state_ :: State
  }
    deriving (Show, Typeable)

state :: Accessor Terminal State
state = accessor state_ (\ a r -> r{state_ = a})

robots :: Accessor Terminal [RobotIndex]
robots = accessor robots_ (\ a r -> r{robots_ = a})

batteryNumber :: Accessor Terminal Integer
batteryNumber = accessor batteryNumber_ (\ a r -> r{batteryNumber_ = a})

showingBubble :: Accessor Terminal Bool
showingBubble = accessor showingBubble_ (\ a r -> r{showingBubble_ = a})

unwrapTerminal :: Object_ -> Maybe Terminal
unwrapTerminal (Object_ _sort o) = cast o

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
    zipRobotsSelected (Just (Uncontrollable _)) _selected =
        Defunct
    zipRobotsSelected Nothing _ =
        Off

    mapRobots :: Maybe RobotIndex -> LightState
    mapRobots (Just (Controllable _)) = On
    mapRobots (Just (Uncontrollable _)) = Defunct
    mapRobots Nothing = Off

    blinkingOut = blinkingMode && even (floor ((now - changedTime state) / blinkLength) :: Int)
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
    sortId TSort{transparent} = SortId $ if transparent
        then "story-mode/transparentTerminal"
        else "terminal"
    sortId BatteryTSort{} = SortId "story-mode/batteryTerminal"
    size TSort{} = standardTerminalSize
    size BatteryTSort{} = batteryTerminalSize
    renderIconified sort ptr =
        case sort of
            TSort{transparent} -> if not transparent then
                renderPixmapSimple ptr $ background $ pixmaps sort
              else
                renderIconifiedTransparent "transparentTerminal"
            BatteryTSort{} ->
                renderIconifiedTransparent "batteryTerminal"
      where
        renderIconifiedTransparent name = do
            fillRect ptr zero (size sort) (alpha ^= 0.3 $ black)
            setPenColor ptr white 1
            drawText ptr (Position 5 15) False name

    objectEditMode _ = Just oemMethods

    initialize _app file (Just space) sort editorPosition (Just (OEMState oemState_)) _ = io $ do
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
    initialize _app _ Nothing sort@TSort{} editorPosition _ _ = do
        let position = epToPosition (size sort) editorPosition
            (_, baryCenterOffset) = mkPolys $ size sort
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        return $ case sort of
            TSort{} -> Terminal chip [] (initialMenuState 0)
            BatteryTSort{} -> StandbyBatteryTerminal chip [] 0 False Nothing

    immutableCopy t =
        CM.immutableCopy (chipmunk t) >>= \ x -> return t{chipmunk = x}

    getControlledChipmunk scene _ =
        getControlledChipmunk scene $
        scene ^. mainLayerObjectA (Base.nikki $ scene ^. mode)

    chipmunks = chipmunk >>> return

    startControl _now t@StandbyBatteryTerminal{} = t
    startControl now t =
        state ^= reset (now - blinkLength) (t ^. robots) (t ^. state) $
        t

    isUpdating = const True

    updateNoSceneChange = update

    renderObject app config terminal sort _ptr offset now = do
        pos <- fst <$> getRenderPositionAndAngle (chipmunk terminal)
        return $ renderTerminal app config sort offset now terminal pos


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
    (Size wh hh_) :: Size CpFloat = fmap realToFrac $ fmap (/ 2) size
    hh = hh_ - topCutoff / 2
    baryCenterOffset = Vector wh (hh + topCutoff)
    topCutoff = fromUber 8
    -- the physics object is much narrower to simulate,
    -- nikki being able to use the terminal if nikki is completely
    -- inside the terminal frame. (With height, there is no such padding,
    -- as nikki can only jump when standing on the ground.)
    padding = realToFrac $ width nikkiSize


-- * controlling

update _sort _ config _ _scene now contacts (False, cd) terminal@StandbyBatteryTerminal{} =
    return $
    updateShowingBubble (config ^. controls) contacts cd $
    updateUncontrolledStandby now $
    terminal
update sort _ config _ scene now _contacts (True, _cd) terminal@StandbyBatteryTerminal{} =
    if terminal ^. showingBubble then
        -- already showing the bubble
        return $
        showingBubble ^= False $
        terminal
      else
        updateOnTime config now sort .
        (showingBubble ^= True) =<<
        putBatteriesInTerminal config scene now sort terminal
update _sort _ _config _ scene _now contacts (False, _cd) terminal =
    (state ^: updateGameMode contacts terminal) <$>
    (robots ^^: updateControllableStates scene) terminal
update _sort _ config _ scene now _contacts (True, cd) terminal =
    (robots ^^: updateControllableStates scene) $
    state ^= updateState (config ^. controls) now cd (terminal ^. robots) (terminal ^. state) $
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
updateState config _now cd robots state | isTerminalConfirmationPressed config cd =
  case row state of
    NikkiState -> exitToNikki state
    RobotState -> case (robots !! robotIndex state) of
        Controllable i -> state{exitMode = ExitToRobot i}
        Uncontrollable _i -> state -- TODO: sound
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
    updateUncontrolledStandby 0 <$> case file of
        (EpisodeLevel episode _ _ _ _) -> do
            batteries <- batteriesInTerminal <$> getEpisodeScore (euid episode)
            let onTime = if batteries >= batteryNumberNeeded then Just (- bootingAnimationTime) else Nothing
            return $ StandbyBatteryTerminal chip robots batteries False onTime
        -- not in story-mode (shouldn't happen at all, just for testing)
        _ -> return $ StandbyBatteryTerminal chip robots 0 False Nothing


putBatteriesInTerminal :: Configuration -> Scene o -> Seconds -> TSort -> Terminal -> IO Terminal
putBatteriesInTerminal config scene _now BatteryTSort{insertionSound} t@StandbyBatteryTerminal{} = do
    case levelFile scene of
        (EpisodeLevel episode _ _ _ _) -> do
            _score <- getEpisodeScore $ euid episode
            batteries <- getCollectedBatteries episode
            setEpisodeScore (euid episode) (EpisodeScore_0 True batteries)
            when (batteries < batteryNumberNeeded) $
                triggerSound config insertionSound
            return $ batteryNumber ^= batteries $ t
        -- for testing in normal mode
        _ -> return $ batteryNumber ^= (scene ^. batteryPower .> firstAStrict) $ t

getCollectedBatteries :: Episode LevelFile -> IO Integer
getCollectedBatteries episode =
    getHighScores >$> \ hs ->
        -- insert the actual collected batteries for the current level
    -- TODO: pretend, there are no batteries in the outro level for now
    sumOfEpisodeBatteries hs episode

bootingAnimationTime = bootingFrameTime * fromIntegral bootingAnimationSteps

-- | sets the onTime field if appropriate
updateOnTime :: Configuration -> Seconds -> TSort -> Terminal -> IO Terminal
updateOnTime config now BatteryTSort{bootingSound} t@StandbyBatteryTerminal{..} = case onTime of
    Nothing -> if batteryNumber_ >= batteryNumberNeeded
        then do
            triggerSound config bootingSound
            return t{onTime = Just now}
        else return t
    Just _x -> return t

-- | changes from Standby to BatteryTerminal if appropriate
updateUncontrolledStandby :: Seconds -> Terminal -> Terminal
updateUncontrolledStandby now t@StandbyBatteryTerminal{..} = case onTime of
    Nothing -> t
    Just onTime ->
        if onTime + bootingAnimationTime <= now
        then BatteryTerminal chipmunk robots_ (initialMenuState now)
        else t

updateShowingBubble :: Controls -> Contacts -> ControlData -> Terminal -> Terminal
updateShowingBubble _config contacts _cd terminal =
    if stillTouching then
        -- touching -> do nothing
        terminal
      else
        showingBubble ^= False $
        terminal
  where
    stillTouching = fany (hasTerminalShape terminal) (terminals contacts)


-- * game rendering

renderTerminal _ _ sort@TSort{transparent} _ now t pos =
    renderTerminalBackground transparent sort pos ++
    renderDisplayBlinkenLights sort now t pos ++
    RenderPixmap (colorBar $ pixmaps sort) (pos +~ colorBarOffset) Nothing :
    catMaybes (renderLittleColorLights sort now t pos)
renderTerminal app config sort@BatteryTSort{} offset now t pos =
    renderDisplayBlinkenLights sort now t pos ++
    renderBatteryBar sort now t pos ++
    renderBootingAnimation sort t now pos ++
    catMaybes (renderLittleColorLights sort now t pos) ++
    renderTerminalSpeechBubble app config offset sort pos t ++
    []

colorBarOffset = fmap fromUber $ Position 30 20

renderTerminalBackground transparent sort pos =
    if transparent
    then []
    else singleton $ RenderPixmap (background $ pixmaps sort) pos Nothing

-- | renders the main terminal pixmap (with blinkenlights)
renderDisplayBlinkenLights _ _ StandbyBatteryTerminal{} _ =
    [] -- don't render anything in standby mode
renderDisplayBlinkenLights sort now _ pos =
    let pixmap = pickAnimationFrame (blinkenLights $ pixmaps sort) now
    in singleton $ RenderPixmap pixmap (pos +~ blinkenLightOffset) Nothing

blinkenLightOffset = fmap fromUber $ Position 13 18

-- | renders the little colored lights (for the associated robots) on the terminal in the scene
renderLittleColorLights _ _ StandbyBatteryTerminal{} _ =
    [] -- don't render anything in standby mode
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

firstBeamOffset = fmap fromUber $ Position 55 32

-- | offset from one beam to the next
beamIncrement = Position 0 (- fromUber 2)

-- | number of beams including the top
numberOfBeams = 9

beamBlinkingAnimation = mkAnimation [True, False] [beamBlinkingTime]

renderBatteryBar sort@BatteryTSort{..} now StandbyBatteryTerminal{batteryNumber_} p =
    case roundToBars numberOfBeams batteryNumberNeeded batteryNumber_ of
        0 -> if pickAnimationFrame beamBlinkingAnimation now
             then singleton $ RenderPixmap whiteBeam (p +~ firstBeamOffset) Nothing
             else []
        n -> renderGreenBeams sort n p
renderBatteryBar sort@BatteryTSort{..} _now BatteryTerminal{} p =
    renderGreenBeams sort numberOfBeams p

roundToBars :: Integer -> Integer -> Integer -> Integer
roundToBars numberOfBeams batteryNumberNeeded n | n >= batteryNumberNeeded =
    numberOfBeams
roundToBars _numberOfBeams _batteryNumberNeeded n | n <= 0 =
    0
roundToBars numberOfBeams batteryNumberNeeded n =
    succ $ floor
        ((fromIntegral (pred n * pred numberOfBeams) / fromIntegral (pred batteryNumberNeeded)) :: Double)

renderGreenBeams BatteryTSort{..} (n :: Integer) p =
    map inner [1 .. n]
  where
    inner n =
        let (pixmap, offset) = pix n
        in RenderPixmap
            pixmap
            (p +~ offset)
        Nothing
    pix n =
        if n < numberOfBeams then
            (greenBeam, firstBeamOffset +~ fmap (* fromIntegral (n - 1)) beamIncrement)
          else if n == numberOfBeams then
            (greenTop, firstBeamOffset +~ Position 0 (- fromUber 17))
          else
            error "renderGreenBeams"

renderBootingAnimation BatteryTSort{..} StandbyBatteryTerminal{onTime} now p = case onTime of
    Nothing -> if pickAnimationFrame beamBlinkingAnimation now
        then singleton $ RenderPixmap bootingStandBy (p +~ colorBarOffset) Nothing
        else []
    Just onTime ->
        let pix = pickAnimationFrame bootingPixmaps (now - onTime)
        in singleton $ RenderPixmap pix (p +~ colorBarOffset) Nothing
renderBootingAnimation BatteryTSort{..} BatteryTerminal{} _now p =
    singleton $ RenderPixmap (colorBar $ pixmaps tsort) (p +~ colorBarOffset) Nothing

renderTerminalSpeechBubble app config offset sort terminalPos
  terminal@StandbyBatteryTerminal{onTime} =
    if terminal ^. showingBubble && isNothing onTime then
        let context = ("supplied", show (terminal ^. batteryNumber)) :
                      ("needed", show batteryNumberNeeded) :
                      []
            glyphs = SL.fromList $ map SL.fromList $ concat $
                fmap (wordWrap (standardFont app) bubbleTextWidths .
                      capitalizeProse .
                      substitute context .
                      p) $
                ("Supplied batteries: ${supplied}/${needed}." :
                 "EOL" :
                 [])
        in singleton $ renderSpeechBubble app config offset terminalPos (size sort) glyphs
      else
        []
renderTerminalSpeechBubble _ _ _ _ _ _ = []


-- * rendering of game OSD

renderTerminalOSD :: Ptr QPainter -> Seconds -> Scene Object_ -> IO ()
renderTerminalOSD ptr now scene@Scene{mode_ = mode@Base.TerminalMode{}} =
    let terminal = Base.terminal mode
        object = scene ^. mainLayerObjectA terminal
        sort = sort_ object
    in case (unwrapTerminalSort sort, unwrapTerminal object) of
        (Just _sort, Just StandbyBatteryTerminal{}) ->
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

renderOsdExit ptr offset _now pixmaps state exitState = do
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
    (\ s -> fmap OEMState (readMay s :: Maybe TerminalOEMState))


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
editorUpdate _ (KeyboardButton key _ _) NoRobots | key `elem` keys = return NoRobots
  where
    keys = (RightArrow : LeftArrow : Return : Enter : [])
editorUpdate _ _ NoRobots = oemNothing
editorUpdate _scene (KeyboardButton key _ _) state@(Robots available selected attached) =
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
removeDeletedRobots _scene NoRobots = NoRobots
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
oemCursor scene (Robots _available selected _) =
    getMainLayerEditorObject scene selected ^. editorPosition

renderOEMOSDs :: Sort sort o => Ptr QPainter -> Offset Double -> EditorScene sort -> TerminalOEMState
    -> IO ()
renderOEMOSDs _ptr _offset _scene NoRobots = return ()
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
