{-# language NamedFieldPuns, ScopedTypeVariables #-}

module Game.Scene (
    Scene,
    UnloadedScene,
    UninitializedScene,
    Scene_(..),

    sceneInitCollisions,
    sceneInitChipmunks,

    stepScene,
  ) where


import Utils

import Data.Indexable (Index, (!!!), fmapMWithIndex)
import Control.Monad.FunctorM
import Data.Abelian

import Control.Monad.State hiding ((>=>), (<=<))
import Control.Monad.Compose

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Base.Events
import Base.Grounds
import Base.Configuration as Configuration

import Objects
import Objects.Types
import qualified Objects.Terminals as Terminals
import Objects.Collisions
import Objects.Animation

import Game.Scene.Types
import Game.Scene.Camera
import qualified Game.Modes.Terminal as TerminalMode




-- * Initialisation

-- | initializes the collisions logick
sceneInitCollisions :: Space -> Scene -> IO Scene
sceneInitCollisions space s@Scene{objects, collisions} = do
    cs' <- initCollisions space (mainLayerIndexable objects) collisions
    return s{collisions = cs'}

-- | initializes all chipmunk objects in the MainLayer and all Animations
sceneInitChipmunks :: Space -> UninitializedScene -> IO Scene
sceneInitChipmunks space scene@Scene{objects, osdSpriteds} = do
    let Grounds bgs mainLayer fgs = objects

    mainLayer' <- fmapM (pure objectInitAnimation <=< objectInitChipmunk scene space) mainLayer

    let initDummy = objectInitAnimation . initDummyChipmunk
        bgs' = fmap (fmap initDummy) bgs
        fgs' = fmap (fmap initDummy) fgs
        objects' = Grounds bgs' mainLayer' fgs'
        osdSpriteds' = fmap initDummy osdSpriteds
    return $ scene{objects = objects', osdSpriteds = osdSpriteds'}



-- * entry

-- order of one frame step is this:
-- maybe transition to another scene
-- chipmunk stepping
-- updating the objects
-- rendering

stepScene :: Seconds -> Space -> ControlData -> Ptr QPainter -> Scene -> IO Scene
stepScene now space controlData ptr =
    updatePassedTime now >=>
    updateScene controlData >=>
    passThrough (stepSpace space) >=>
    renderScene ptr >=>
    pure (maybeId (flip transition controlData))

-- * step time measurement

-- | updates the passedTime variable
-- time passes in normal Game mode
-- time does NOT pass in Terminal mode
updatePassedTime :: Seconds -> Scene -> IO Scene
updatePassedTime now scene@Scene{now = oldNow} = do
    let passedTime = now - oldNow
    return scene{now = now, passedTime = passedTime}
updatePassedTime now scene@TerminalMode{innerScene} = do
    let innerScene' = innerScene{now = now, passedTime = 0}
    return scene{innerScene = innerScene'}


-- * State automaton stuff

transition :: Scene -> ControlData -> (Maybe Scene)
transition scene (ControlData pushed _) = runHandler [
    nikkiToTerminal scene pushed,
    terminalToNikki scene pushed,
    terminalToRobots scene pushed,
    robotsToTerminal scene pushed,
    gameOver scene,
    levelPassed scene
  ]

-- | converts the Scene to TerminalMode, if appropriate
nikkiToTerminal :: Scene -> [AppEvent] -> (Maybe Scene)
nikkiToTerminal scene@Scene{controlled} pushed
  | bPressed && isNikkiMode scene && beforeTerminal =
    Just $ TerminalMode scene{controlled = terminal} terminal robots
  where
    bPressed = Press BButton `elem` pushed
    beforeTerminal = nikkiTouchesTerminal $ collisions scene

    terminal = whichTerminalCollides $ collisions scene
    robots = Terminals.terminalRobots $ terminalState
                (mainLayerIndexable (objects scene) !!! terminal)
nikkiToTerminal _ _ = Nothing

terminalToNikki :: Scene -> [AppEvent] -> (Maybe Scene)
terminalToNikki scene@TerminalMode{} pushed
    | bReleased && nikkiSelected =
    Just $ innerScene'{controlled = nikki innerScene', mTerminal = Nothing}
  where
    bReleased = Release BButton `elem` pushed
    nikkiSelected = not $ Terminals.isRobotSelected $ terminalState $ getControlled scene
    innerScene' = innerScene scene
terminalToNikki _ _ = Nothing

terminalToRobots :: Scene -> [AppEvent] -> (Maybe Scene)
terminalToRobots scene@TerminalMode{innerScene, robots, terminal} pushed
    | bReleased && robotSelected =
    Just innerScene{controlled = robotIndex, mTerminal = Just terminal}
  where
    bReleased = Release BButton `elem` pushed
    robotSelected = Terminals.isRobotSelected $ terminalState $ getControlled scene
    robotIndex =
        robots !!
            (Terminals.terminalSelected (terminalState (sceneGetMainObject innerScene terminal)))
terminalToRobots _ _ = Nothing

-- | converts from RobotMode to TerminalMode, if appropriate
robotsToTerminal :: Scene -> [AppEvent] -> (Maybe Scene)
robotsToTerminal scene@Scene{mTerminal = Just terminal} pushed
  | bPressed && isRobotMode scene =
    Just $ initTheLights $
      modifyControlled (modifyTerminalState (Terminals.modifyIsRobotSelected (const False))) $
        TerminalMode scene{controlled = terminal} terminal robots
  where
    bPressed = Press BButton `elem` pushed

    robots = Terminals.terminalRobots $ terminalState
                (mainLayerIndexable (objects scene) !!! terminal)

    -- | switch TerminalLights to initialLights
    initTheLights = modifySelectedTerminal initialTerminalLights
robotsToTerminal _ _ = Nothing

gameOver :: Scene -> Maybe Scene
gameOver scene@Scene{collisions} | nikkiTouchesLaser collisions =
    Just $ FinalState FailedLevel
gameOver _ = Nothing

levelPassed :: Scene -> Maybe Scene
levelPassed scene@Scene{collisions} | nikkiTouchesMilkMachine collisions =
    Just $ FinalState PassedLevel
levelPassed _ = Nothing


-- * chipmunk stepping

stepSpace :: Space -> Scene -> IO ()

stepSpace space s@Scene{passedTime} = do
    when (passedTime < 0.1) $ -- TODO: dirty hack
        CM.step space (passedTime) -- <<? "passedTime")

stepSpace space s@Scene{passedTime} = do
    -- in order for step times to be of equal length, we invoke CM.step multiple times
    -- with a "time quantum" (singleStepTime)
    -- this (hoefully) increases chipmunk performance
    let nos :: Int = truncate (passedTime / singleStepTime)
    forM_ [0..nos] $ const $
        CM.step space singleStepTime
stepSpace _ s@TerminalMode{} = return ()

singleStepTime :: Fractional n => n
singleStepTime = 0.1

-- * object updating

-- | updates every object
updateScene :: ControlData -> Scene -> IO Scene
updateScene cd scene@Scene{now, objects, controlled, collisions} = do
    -- TODO: which order?
    collisions' <- updateCollisions (mainLayerIndexable objects) collisions
    objects' <- sceneUpdateObjects collisions' now cd controlled scene objects
    return $ scene{objects = objects', collisions = collisions'}

updateScene cd scene@TerminalMode{innerScene} = do
    let Scene{now, objects, controlled, collisions} = innerScene
    -- TODO: which order?
    collisions' <- updateCollisions (mainLayerIndexable objects) collisions
    objects' <- sceneUpdateObjects collisions' now cd controlled scene objects
    let innerScene' = innerScene{objects = objects', collisions = collisions'}

    return scene{innerScene = innerScene'}


-- | updates all objects
sceneUpdateObjects :: Collisions -> Seconds -> ControlData -> Index -> Scene
    -> Grounds Object -> IO (Grounds Object)
sceneUpdateObjects collisions now cd controlled scene grounds = do
    let (Grounds backgrounds mainLayer foregrounds) = grounds

        -- update function for all objects in the mainLayer
        updateMainObjects :: Index -> Object -> IO Object
        updateMainObjects i o = updateObject scene now collisions (i == controlled, cd) o
        -- update function for updates outside the mainLayer
        updateMultiLayerObjects :: Object -> IO Object
        updateMultiLayerObjects o = updateObject scene now collisions (False, cd) o

    backgrounds' <- fmapM (fmapM updateMultiLayerObjects) backgrounds
    -- each object has to know, if it's controlled
    mainLayer' <- modifyContentM (fmapMWithIndex updateMainObjects) mainLayer
    foregrounds' <- fmapM (fmapM updateMultiLayerObjects) foregrounds

    return $ Grounds backgrounds' mainLayer' foregrounds'



-- * rendering

-- | well, renders the scene to the screen (to the max :)
renderScene :: Ptr QPainter -> Scene -> IO Scene
renderScene ptr scene@Scene{} = do

    resetMatrix ptr
    windowSize <- sizeQPainter ptr
    eraseRect ptr zero windowSize (QtColor 0 0 0 255)

    (center, cameraState') <- getCenter (getControlled scene) (cameraState scene)

    intSize@(Size width height) <- sizeQPainter ptr
    let size = fmap fromIntegral intSize
        offsetVector = - (center - Vector (fromIntegral width / 2) (fromIntegral height / 2))
        offset = fmap (fromIntegral . truncate) $ vectorToPosition offsetVector

    when (showScene Configuration.development) $ do
        -- TODO: this is a workaround for padding errors!!!
        let os = objects scene
            multiLayerOffset = offset -~ Position 1 1
        fmapM_ (renderLayer ptr size multiLayerOffset scene) $ backgrounds os
        renderLayer ptr size offset scene $ mainLayer os
        fmapM_ (renderLayer ptr size multiLayerOffset scene) $ foregrounds os
--             layerMapM_ (renderLayer ptr size offset scene) $ objects scene

    when (showGrid Configuration.development) $ do
--             renderLayer ptr size offset scene $ mainLayer $ objects scene
        fmapM_ (renderObjectGrid ptr offset) $ mainLayer $ objects scene



    return scene{cameraState = cameraState'}

renderScene ptr scene@TerminalMode{innerScene} = do
    innerScene' <- renderScene ptr innerScene
    let scene' = scene{innerScene = innerScene'}
    TerminalMode.renderOSD ptr scene'
    return scene'

-- | renders the different Layers.
-- makes sure, everything is rendered ok.
renderLayer :: Ptr QPainter -> Size Double -> Qt.Position Double -> Scene -> Layer Object -> IO ()
renderLayer ptr size offset scene layer = do
    let modifiedOffset = calculateLayerOffset size offset layer
    fmapM_ (render ptr modifiedOffset scene) (content layer)


-- * debugging

debugDrawCoordinateSystem :: Ptr QPainter -> CM.Position -> IO ()
debugDrawCoordinateSystem ptr (Vector x y) = do
    resetMatrix ptr
    translate ptr (Position x y)
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


debugScene :: Scene -> IO ()
debugScene s = do
    let os = objects s
    fmapM_ inner os
  where
    inner o = do
        p <- getObjectPosition o
        print (p, take 10 (show o))

renderObjectGrid :: Ptr QPainter -> Qt.Position Double -> Object -> IO ()

renderObjectGrid ptr offset object = do
    resetMatrix ptr
    translate ptr offset
    renderGrid ptr (chipmunk object)

-- renderObjectGrid ptr offset o = es "renderGrid" o

