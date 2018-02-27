{-# language ViewPatterns, ScopedTypeVariables, NamedFieldPuns #-}

module Game.Scene (
    Scene,
    stepScene,
    RenderStateRefs(..),
    SceneMVar,
    mkRenderState,
    sceneImmutableCopy,
    renderScene,
  ) where

import Prelude hiding (foldr)

import Data.Indexable (Indexable(..), Index, findIndices, toList, indexA)
import qualified Data.Indexable.Range as Range
import qualified Data.Set as Set
import Data.Abelian
import Data.IORef

import Text.Printf

import Control.Monad (join)
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State.Strict as StrictState

import Control.Concurrent.MVar

import Graphics.Qt as Qt

import Physics.Chipmunk as CM hiding (renderPosition)

import Utils

import Base
import Base.Renderable.StickToBottom

import qualified Profiling.Physics
import Profiling.FPS (FPSRef, tickFPSRef, initialFPSRef)

import Object

import Game.Scene.Camera
import Game.Scene.OptimizeRenderPixmaps

import Sorts.Nikki.Types
import Sorts.Terminal
import Sorts.LowerLimit


-- * entry

-- order of one frame step is this:
-- maybe transition to another scene
-- chipmunk stepping
-- updating the objects
-- rendering

stepScene :: Application -> Configuration -> Space -> ControlData
    -> Scene Object_ -> IO (Scene Object_)
stepScene app configuration space controlData =
    updateScene app space configuration controlData >=>
    stepSpace space >=>
    maybeAbort configuration >=>
    transition (configuration ^. controls) controlData


-- * State automaton stuff

transition :: Controls -> ControlData -> Scene Object_ -> IO (Scene Object_)
transition controls cd scene = do
    controlledPosition :: Maybe CM.Position <-
        fmapM (getPosition . getControlledChipmunk scene) $ getControlled scene
    case mNew controlledPosition of
        Nothing -> return scene
        Just new -> modifyTransitioned new
  where
    -- | Maybe the new scene
    mNew :: Maybe CM.Position -> Maybe (Scene Object_)
    mNew controlledPosition = foldl1 (<|>) [
        nikkiToTerminal controls scene cd,
        terminalExit scene,
        robotToTerminal controls scene cd,
        nikkiMovedAwayFromTerminal scene,
        lowerLimitHandler scene controlledPosition,
        touchesDeadlyHandler scene,
        levelPassed scene
      ]

-- | applied to the scene after every transition
modifyTransitioned :: Scene Object_ -> IO (Scene Object_)
modifyTransitioned scene = do
    resetHeldKeys
    return $ case getControlledIndex scene of
      Just controlledIndex ->
        let now = scene ^. spaceTime
        in objects .> gameMainLayer .> indexA controlledIndex ^:
            (startControl now) $ scene
      Nothing -> scene


-- | converts the Scene to TerminalMode, if appropriate
nikkiToTerminal :: Controls -> Scene Object_ -> ControlData -> Maybe (Scene Object_)
nikkiToTerminal config scene@Scene{mode_ = (NikkiMode nikkiIndex)} cd
                                               -- nikki must be in wait mode
    | contextButtonPressed && beforeTerminal && waiting
        = Just $ mode ^= mode' $ scene
  where
    contextButtonPressed = isGameContextPressed config cd
    beforeTerminal = nikkiTouchesTerminal $ scene ^. contacts
    nikki :: Nikki
    Just nikki = unwrapNikki $ scene ^. mainLayerObjectA nikkiIndex
    action_ = action $ state $ nikki
    waiting = isWaitAction action_

    mode' = TerminalMode nikkiIndex terminal
    (terminal : _) = whichTerminalCollides scene
nikkiToTerminal _ _ _ = Nothing


whichTerminalCollides :: Scene Object_ -> [Index]
whichTerminalCollides scene =
    findIndices p allTerminals
  where
    allTerminals :: Indexable (Maybe Terminal)
    allTerminals = fmap unwrapTerminal $ scene ^. objects ^. gameMainLayer

    p :: Maybe Terminal -> Bool
    p Nothing = False
    p (Just t) = any (\ shape -> hasTerminalShape t shape) collidingShapes
    collidingShapes :: [Shape]
    collidingShapes = (scene ^. contacts) |> terminals |> Set.toList

terminalExit :: Scene Object_ -> Maybe (Scene Object_)
terminalExit scene@Scene{mode_ = TerminalMode{nikki, terminal}} =
    case unwrapTerminal $ scene ^. mainLayerObjectA terminal of
        Just t -> case terminalExitMode t of
            DontExit -> Nothing
            ExitToNikki ->
                Just $ mode ^= NikkiMode nikki $ scene
            ExitToRobot robot ->
                Just $ mode ^= RobotMode nikki terminal robot $ scene
terminalExit _ = Nothing


-- | converts from RobotMode to TerminalMode, if appropriate
robotToTerminal :: Controls -> Scene Object_ -> ControlData -> Maybe (Scene Object_)
robotToTerminal config scene@Scene{mode_ = RobotMode{nikki, terminal}} cd
  | bPress =
    Just $ mode ^= TerminalMode nikki terminal $ scene
  where
    bPress = isRobotBackPressed config cd
robotToTerminal _ _ _ = Nothing

-- | if nikki gets moved away from the terminal during robot mode...
nikkiMovedAwayFromTerminal :: Scene Object_ -> Maybe (Scene Object_)
nikkiMovedAwayFromTerminal scene@Scene{mode_} =
    if isRobotMode mode_ || isTerminalMode mode_ then
        if terminal mode_ `elem` whichTerminalCollides scene then
            Nothing
        else
            Just $ mode ^= NikkiMode (nikki mode_) $ scene
    else
        Nothing


touchesDeadlyHandler :: Scene Object_ -> Maybe (Scene Object_)
touchesDeadlyHandler scene | isGameOver =
    Just $ mode ^= (mkLevelFinished scene Failed) $ scene
  where
    now = scene ^. spaceTime
    batteries = scene ^. batteryPower
    isGameOver =
        isGameMode (scene ^. mode)
        && nikkiTouchesDeadly (scene ^. contacts)
touchesDeadlyHandler _ = Nothing

levelPassed :: Scene Object_ -> Maybe (Scene Object_)
levelPassed scene =
    if allTriggered && isGameMode (scene ^. mode) then
        Just $ mode ^: (const $ mkLevelFinished scene Passed) $ scene
      else
        Nothing
  where
    allTriggered = pressed >= total && total > 0
    (pressed :!: total) = scene ^. switches
    now = scene ^. spaceTime
    batteries = scene ^. batteryPower


-- * chipmunk stepping

-- | run steps in the physics simulation and update the contacts
-- leaves contacts untouched, if no simulation steps are being done

stepSpace :: Space -> Scene Object_ -> IO (Scene Object_)
stepSpace space scene@Scene{contactRef} = do
    resetContactRef contactRef
    CM.step space subStepQuantum
    contacts' <- readContactRef contactRef
    return $
        contacts ^= contacts' $
        spaceTime ^: (+ subStepQuantum) $
        scene

-- | aborts the game, when specified by clo --abort_level
maybeAbort :: Configuration -> Scene Object_ -> IO (Scene Object_)
maybeAbort config = case abort_level config of
    Nothing -> return
    Just limit -> \ scene ->
        if scene ^. spaceTime >= limit then error "level aborted" else return scene


-- * object updating

-- | updates every object
updateScene :: Application -> Space -> Configuration -> ControlData
    -> Scene Object_ -> IO (Scene Object_)
updateScene app space config cd scene = do
    -- NOTE: Currently only the physics layer is updated
    let mainLayerUpdatingRange = gameMainLayerUpdatingRange (scene ^. objects)
    (physicsContent', sceneChange) <- updateMainLayer mainLayerUpdatingRange $ scene ^. objects ^. gameMainLayer
    return $ sceneChange $ objects .> gameMainLayer ^= physicsContent' $ scene
  where
    now = scene ^. spaceTime
    controlled = getControlledIndex scene

    -- update function for all objects in the mainLayer
    updateMainLayer :: Range.Range -> Indexable Object_
        -> IO (Indexable Object_, Scene Object_ -> Scene Object_)
    -- each object has to know, if it's controlled
    updateMainLayer mainLayerUpdatingRange ix = do
        StrictState.runStateT
            (Range.fmapMWithIndex mainLayerUpdatingRange (\ i o ->
                update DummySort app config space scene now (scene ^. contacts)
                    (Just i == controlled, cd) i o)
                ix)
            id


-- * rendering

-- | type for rendering the scene
data RenderStateRefs
    = RenderStateRefs {
        controlsStateRefs :: Controls,
        sceneMVar :: SceneMVar,
        cameraStateRef :: IORef CameraState,
        fpsRef :: FPSRef
    }

type SceneMVar = MVar (Scene Object_, DebuggingCommand)

-- | RenderScene (for the rendering thread)
-- Gets created in the logic thread
mkRenderState :: IORef CameraState -> Scene Object_ -> M RenderStateRefs
mkRenderState cameraStateRef scene = do
    controls_ <- (^. controls) <$> getConfiguration
    let noop :: DebuggingCommand = const $ const $ return ()
    newScene <- io $ sceneImmutableCopy scene
    sceneMVar <- io $ newMVar (newScene, noop)
    fpsRef <- initialFPSRef

    return $ RenderStateRefs controls_ sceneMVar cameraStateRef fpsRef

sceneImmutableCopy :: Scene Object_ -> IO (Scene Object_)
sceneImmutableCopy scene = do
    let old = scene ^. acc
    new <- io $ fmapM Base.immutableCopy old
    return $ acc ^= new $ scene
  where
    acc = objects .> gameMainLayer

-- let renderable_ = renderable (fpsRef, cameraStateRef :: IORef CameraState, sceneMVar :: MVar (RenderScene, DebuggingCommand))
instance Renderable RenderStateRefs where
    label = const "RenderScene"
    render ptr app config size (RenderStateRefs controls sceneMVar cameraStateRef fpsRef) = do
        return $ tuple size $ do
            (scene, debugging) <- readMVar sceneMVar
            runStateTFromIORef cameraStateRef $
                Game.Scene.renderScene app config ptr scene debugging
            when (config ^. show_keyhint_OSD) $
                renderKeysHint (scene ^. mode)

            tickFPSRef app config ptr fpsRef
      where
        renderKeysHint :: Mode -> IO ()
        renderKeysHint mode = do
            let mr = fmap (stickToBottom () . keysHintRenderable) $ case mode of
                        NikkiMode{} -> Just $ gameNikkiKeysHint controls
                        TerminalMode{} -> Just $ gameTerminalKeysHint controls
                        RobotMode{} -> Just $ gameRobotKeysHint controls
                        _ -> Nothing
            forM_ mr $ \ r -> do
                resetMatrix ptr
                join (snd <$> render ptr app config size r)


-- | Well, renders the scene to the screen (to the max :)
-- Happens in the rendering thread.
renderScene :: Application -> Configuration -> Ptr QPainter
    -> Scene Object_ -> DebuggingCommand -> StateT CameraState IO ()
renderScene app config ptr scene debugging = do
    let now = scene ^. spaceTime
    center <- getCameraPosition ptr scene
    io $ do
        size <- sizeQPainter ptr
        let Size width height :: Size CpFloat = fmap realToFrac size
            offsetVector = - (center - Vector (width / 2) (height / 2))
            offset = fmap (fromIntegral . truncateInteger) $
              vector2position offsetVector

        clearScreen ptr black

        renderObjects app config size ptr offset now (scene ^. objects)

        renderOSDs app config ptr now scene

        -- debugging
        when (render_xy_cross config) $
            debugDrawCoordinateSystem ptr offset
        when (render_chipmunk_objects config) $
            fmapM_ (renderObjectGrid ptr offset) $ scene ^. objects ^. gameMainLayer
        io $ debugging ptr offset
        Profiling.Physics.render app config ptr now

renderObjects app config windowSize ptr offset now gameGrounds | not develMode =
    when (not $ omit_pixmap_rendering config) $ do
        renderPixmaps <- gameGroundsToRenderPixmaps app config windowSize ptr offset now gameGrounds
        doRenderPixmaps ptr $ optimize windowSize renderPixmaps
-- development version
renderObjects app config size ptr offset now gameGrounds =
    when (not $ omit_pixmap_rendering config) $ do
        renderPixmaps <- gameGroundsToRenderPixmaps app config size ptr offset now gameGrounds
        let fakeSize = Size 800 600
            fakeOffset = size2position $ fmap (/ 2) (size -~ fakeSize)
            fakeMod = fmap (renderPosition ^: (+~ fakeOffset))
        doRenderPixmaps ptr $ fakeMod $ optimize fakeSize renderPixmaps
        resetMatrix ptr
        setPenColor ptr (alpha ^= 0.5 $ red) 1
        drawRect ptr fakeOffset fakeSize

develMode = False

gameGroundsToRenderPixmaps :: Application -> Configuration
    -> Size Double -> Ptr QPainter -> Offset Double -> Seconds
    -> GameGrounds Object_ -> IO [RenderPixmap]
gameGroundsToRenderPixmaps app config size ptr offset now (GameGrounds backgrounds mainLayer _ foregrounds) = do
    bgs <- layersToRenderPixmaps app config size ptr offset now backgrounds
    ml <- mainLayerToRenderPixmaps app config ptr offset now mainLayer
    fgs <- layersToRenderPixmaps app config size ptr offset now foregrounds
    return (bgs ++ ml ++ fgs)

layersToRenderPixmaps :: Application -> Configuration
    -> Size Double -> Ptr QPainter -> Offset Double -> Seconds
    -> [GameLayer Object_] -> IO [RenderPixmap]
layersToRenderPixmaps app config size ptr offset now layers =
    concat <$> fmapM (layerToRenderPixmaps app config size ptr offset now) layers

layerToRenderPixmaps :: Application -> Configuration
    -> Size Double -> Ptr QPainter -> Offset Double -> Seconds
    -> GameLayer Object_ -> IO [RenderPixmap]
layerToRenderPixmaps app config size ptr offset now layer =
    fmap (renderPosition ^: (+~ layerOffset)) <$>
        concat <$> fmapM (\ o -> renderObject_ app config o ptr layerOffset now) (layer ^. gameContent)
  where
    layerOffset =
        calculateLayerOffset size offset (gameXDistance layer, gameYDistance layer)

-- | rendering of the different Layers.
mainLayerToRenderPixmaps :: Application -> Configuration
    -> Ptr QPainter -> Offset Double -> Seconds
    -> Indexable Object_ -> IO [RenderPixmap]
mainLayerToRenderPixmaps app config ptr offset now objects =
    fmap (renderPosition ^: (+~ offset)) <$>
        concat <$> fmapM (\ o -> renderObject_ app config o ptr offset now) (toList objects)


-- * OSDs

renderOSDs app configuration ptr now scene = do
    windowSize <- sizeQPainter ptr

    when (configuration ^. show_time_OSD) $
        renderGameTime ptr app configuration windowSize now

    when (configuration ^. show_battery_OSD) $
        renderBatteryOSD ptr app configuration windowSize scene

    when (configuration ^. show_switch_OSD) $
        renderSwitchesOSD ptr app configuration windowSize scene

    renderTerminalOSD ptr now scene

renderBatteryOSD ptr app configuration windowSize scene = do
    let (collected :!: total) = scene ^. batteryPower
        text = pv $ printf
            (batteryChar : zeroSpaceChar : "%03i/%03i")
            collected
            total
    renderOSD ptr app configuration windowSize text $ \ osdSize ->
        Position (width windowSize - osdPadding - width osdSize) osdPadding

renderGameTime ptr app configuration windowSize now = do
    let text = pv $ (watchChar : zeroSpaceChar : timeFormat now)
    renderOSD ptr app configuration windowSize text $ \ osdSize ->
        fmap (fromIntegral . floorInteger) $
        Position ((width windowSize - width osdSize) / 2) osdPadding

renderSwitchesOSD ptr app configuration windowSize scene = do
    let (pressed :!: total) = scene ^. switches
        text = pv $ printf (switchChar : zeroSpaceChar : "%02i/%02i") pressed total
    renderOSD ptr app configuration windowSize text $
        const $ Position osdPadding osdPadding

renderOSD ptr app configuration windowSize text offsetFun = do
    (osdSize, action) <- Base.render ptr app configuration windowSize (gameOsd text)
    resetMatrix ptr
    translate ptr $ offsetFun osdSize
    action


-- * debugging

debugDrawCoordinateSystem :: Ptr QPainter -> Offset Double -> IO ()
debugDrawCoordinateSystem ptr offset = do
    resetMatrix ptr
    translate ptr offset
    setPenColor ptr red 1
    mapM_ inner lines
  where
    inner (a, b, c, d) =
        drawLine ptr (Position a b) (Position c d)
    big = 1000000
    halfScaleLine = 10
    lines =
        (- big, 0, big, 0) :
        (0, - big, 0, big) :
        scale
    scale = map (\ p -> (p, - halfScaleLine, p, halfScaleLine)) scalePositions ++
            map (\ p -> (- halfScaleLine, p, halfScaleLine, p)) scalePositions
    scalePositions = map (* 64) [-100 .. 100]

renderObjectGrid :: Ptr QPainter -> Qt.Position Double -> Object_ -> IO ()
renderObjectGrid ptr offset object = do
    let chips :: [Chipmunk] = chipmunks object
    renderGrids ptr offset chips
