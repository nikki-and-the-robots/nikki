{-# language NamedFieldPuns, ScopedTypeVariables, ViewPatterns #-}

module Game.Scene (
    Scene,
    stepScene,
  ) where


import Data.Indexable (Indexable, Index, fmapMWithIndex, findIndices)
import Data.Abelian
import Data.Dynamic
import qualified Data.Set as Set

import Control.Monad.State hiding ((>=>), (<=<))
import Control.Monad.Compose
import Control.Monad.FunctorM

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base.Events
import Base.Grounds
import Base.Configuration as Configuration
import Base.Constants

import Object.Types
import Object.Contacts

import Game.Scene.Types
import Game.Scene.Camera

import Sorts.Terminal


-- * Initialisation



-- initializes all chipmunk objects in the MainLayer and all Animations
-- sceneInitChipmunks :: Space -> Scene -> IO Scene
-- sceneInitChipmunks space scene@Scene{objects, osdSpriteds} = do
--     e "sceneInitChipmunks"
--     let Grounds bgs mainLayer fgs = objects
-- 
--     mainLayer' <- fmapM (pure objectInitAnimation <=< objectInitChipmunk scene space) mainLayer
-- 
--     let initDummy = objectInitAnimation . initDummyChipmunk
--         bgs' = fmap (fmap initDummy) bgs
--         fgs' = fmap (fmap initDummy) fgs
--         objects' = Grounds bgs' mainLayer' fgs'
--         osdSpriteds' = fmap initDummy osdSpriteds
--     return $ scene{objects = objects', osdSpriteds = osdSpriteds'}



-- * entry

-- order of one frame step is this:
-- maybe transition to another scene
-- chipmunk stepping
-- updating the objects
-- rendering

stepScene :: Seconds -> Space -> ControlData -> Ptr QPainter -> Scene -> IO Scene
stepScene now space controlData ptr =
    pure (updateNow now) >=>
    updateScene controlData >=>
    passThrough (stepSpace space) >=>
    renderScene ptr now >=>
    pure (maybeId (flip transition controlData))

-- * step time measurement

-- | updates the fields now and oldNow
updateNow :: Seconds -> Scene -> Scene
updateNow now' scene@Scene{now} =
    scene{now = now', oldNow = now}


-- * State automaton stuff

transition :: Scene -> ControlData -> (Maybe Scene)
transition scene (ControlData pushed _) = runHandler [
    nikkiToTerminal scene pushed,
    terminalExit scene,
    robotToTerminal scene pushed,
    gameOver scene,
    levelPassed scene
  ]

runHandler :: [Maybe Scene] -> Maybe Scene
runHandler (Just scene : _) = Just $ sendStartControl scene
  where
    sendStartControl :: Scene -> Scene
    sendStartControl scene = 
        modifyMainByIndex startControl_ (getControlledIndex scene) scene
runHandler (Nothing : r) = runHandler r
runHandler [] = Nothing


-- | converts the Scene to TerminalMode, if appropriate
nikkiToTerminal :: Scene -> [AppEvent] -> (Maybe Scene)
nikkiToTerminal scene@Scene{mode = (NikkiMode nikki)} pushed
    | bPressed && beforeTerminal
        = Just $ scene {mode = mode'}
  where
    bPressed = Press BButton `elem` pushed
    beforeTerminal = nikkiTouchesTerminal $ (snd (contacts scene) <<? "contacts")

    mode' = TerminalMode nikki terminal
    terminal = whichTerminalCollides scene
nikkiToTerminal _ _ = Nothing


whichTerminalCollides :: Scene -> Index
whichTerminalCollides Scene{objects, contacts} =
    case findIndices p allTerminals of
        (a : _) -> a
  where
    allTerminals :: Indexable (Maybe Terminal)
    allTerminals = fmap dynamicToTerminal $ content $ mainLayer objects

    p :: Maybe Terminal -> Bool
    p Nothing = False
    p (Just t) = any (\ shape -> hasTerminalShape t shape) collidingShapes
    collidingShapes :: [Shape]
    collidingShapes = contacts |> snd |> terminals |> Set.toList

dynamicToTerminal :: Object_ -> Maybe Terminal
dynamicToTerminal = unwrapObject .> fromDynamic

terminalExit :: Scene -> (Maybe Scene)
terminalExit scene@Scene{mode = TerminalMode{nikki, terminal}} =
    case dynamicToTerminal $ getMainObject scene terminal of
        Just t -> case exitMode t of
            DontExit -> Nothing
            ExitToNikki ->
                Just scene{mode = NikkiMode nikki}
            ExitToRobot robot ->
                Just scene{mode = RobotMode nikki terminal robot}
terminalExit _ = Nothing


-- | converts from RobotMode to TerminalMode, if appropriate
robotToTerminal :: Scene -> [AppEvent] -> (Maybe Scene)
robotToTerminal scene@Scene{mode = RobotMode{nikki, terminal}} pushed
  | bPress =
    Just $ scene{mode = TerminalMode nikki terminal}
  where
    bPress = Press BButton `elem` pushed
robotToTerminal _ _ = Nothing

gameOver :: Scene -> Maybe Scene
gameOver scene | nikkiTouchesLaser $ snd $ contacts scene =
    Just $ modifyMode (const $ LevelFinished Failed) scene
gameOver _ = Nothing

levelPassed :: Scene -> Maybe Scene
levelPassed scene | nikkiTouchesMilkMachine $ snd $ contacts scene =
    Just $ modifyMode (const $ LevelFinished Passed) scene
levelPassed _ = Nothing


-- * chipmunk stepping

stepSpace :: Space -> Scene -> IO ()

stepSpace _space Scene{mode = TerminalMode{}} = return ()

stepSpace space s@Scene{now, oldNow} = do
    forM_ [0..n] $ const $
        CM.step space stepQuantum
  where
    n :: Int = stepQuantums now - stepQuantums oldNow
    stepQuantum :: Double = 0.002
    stepQuantums :: Seconds -> Int
    stepQuantums t = truncate (t / stepQuantum)

singleStepTime :: Fractional n => n
singleStepTime = 0.1

-- * object updating

-- | updates every object
updateScene :: ControlData -> Scene -> IO Scene
updateScene cd scene@Scene{now, objects, contacts = (contactRef, contacts)} = do
    -- TODO: which order?
    let controlledIndex = getControlledIndex scene
    contacts' <- peekContacts contactRef
    objects' <- sceneUpdateObjects contacts' now cd controlledIndex scene objects
    return $ scene{objects = objects', contacts = (contactRef, contacts')}


-- | updates all objects
sceneUpdateObjects :: Contacts -> Seconds -> ControlData -> Index -> Scene
    -> Grounds Object_ -> IO (Grounds Object_)
sceneUpdateObjects collisions now cd controlled scene grounds = do
    let (Grounds backgrounds mainLayer foregrounds) = grounds

        -- update function for all objects in the mainLayer
        updateMainObjects :: Index -> Object_ -> IO Object_
        updateMainObjects i o = update_ o now collisions (i == controlled, cd)
        -- update function for updates outside the mainLayer
        updateMultiLayerObjects :: Object_ -> IO Object_
        updateMultiLayerObjects o = update_ o now collisions (False, cd)

    backgrounds' <- fmapM (fmapM updateMultiLayerObjects) backgrounds
    -- each object has to know, if it's controlled
    mainLayer' <- modifyContentM (fmapMWithIndex updateMainObjects) mainLayer
    foregrounds' <- fmapM (fmapM updateMultiLayerObjects) foregrounds

    return $ Grounds backgrounds' mainLayer' foregrounds'



-- * rendering

-- | well, renders the scene to the screen (to the max :)
renderScene :: Ptr QPainter -> Seconds -> Scene -> IO Scene
renderScene ptr now scene@Scene{} = do

    resetMatrix ptr
    windowSize <- sizeQPainter ptr
    eraseRect ptr zero windowSize (QtColor 0 0 0 255)

    controlledPosition <- getPosition $ body $ chipmunk_ $ getControlled scene
    (center, cameraState') <- getCenter controlledPosition (cameraState scene)

    intSize@(Size width height) <- sizeQPainter ptr
    let size = fmap fromIntegral intSize
        offsetVector = - (center - Vector (fromIntegral width / 2) (fromIntegral height / 2))
        offset = fmap (fromIntegral . truncate) $ vectorToQtPosition offsetVector

    when (showScene Configuration.development) $ do
        -- TODO: this is a workaround for padding errors!!!
        let os = objects scene
            multiLayerOffset = offset -~ Position 1 1
        fmapM_ (renderLayer ptr size multiLayerOffset now) $ backgrounds os
        renderLayer ptr size offset now $ mainLayer os
        fmapM_ (renderLayer ptr size multiLayerOffset now) $ foregrounds os
--             layerMapM_ (renderLayer ptr size offset scene) $ objects scene


    when (showGrid Configuration.development) $ do
        debugDrawCoordinateSystem ptr offset
--             renderLayer ptr size offset scene $ mainLayer $ objects scene
        fmapM_ (renderObjectGrid ptr offset) $ mainLayer $ objects scene



    return scene{cameraState = cameraState'}

-- renderScene ptr scene@TerminalMode{innerScene} = do
--     innerScene' <- renderScene ptr innerScene
--     let scene' = scene{innerScene = innerScene'}
--     TerminalMode.renderOSD ptr scene'
--     return scene'

-- | renders the different Layers.
-- makes sure, everything is rendered ok.
renderLayer :: Ptr QPainter -> Size Double -> Offset -> Seconds
    -> Layer Object_ -> IO ()
renderLayer ptr size offset now layer = do
    let modifiedOffset = calculateLayerOffset size offset layer
    fmapM_ (\ o -> render_ o ptr modifiedOffset now) (content layer)


-- * debugging

debugDrawCoordinateSystem :: Ptr QPainter -> Offset -> IO ()
debugDrawCoordinateSystem ptr offset = do
    resetMatrix ptr
    translate ptr offset
    setPenColor ptr 255 0 0 255
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
    resetMatrix ptr
    translate ptr offset
    let chip = chipmunk_ object
    renderGrid ptr chip

-- renderObjectGrid ptr offset o = es "renderGrid" o

