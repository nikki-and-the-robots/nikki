{-# language NamedFieldPuns, ScopedTypeVariables, ViewPatterns #-}

module Game.Scene (
    Scene,
    stepScene,
  ) where

import Prelude hiding (foldr)

import Data.Indexable (Indexable, Index, findIndices, fmapMWithIndex)
import Data.Abelian
import qualified Data.Set as Set
import Data.Foldable (foldr)

import Control.Monad.State hiding ((>=>), (<=<))

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base.Events
import Base.Grounds
import Base.Configuration as Configuration
import Base.Constants
import Base.Types

import Object

import Game.Scene.Camera

import Sorts.Terminal


-- * entry

-- order of one frame step is this:
-- maybe transition to another scene
-- chipmunk stepping
-- updating the objects
-- rendering

stepScene :: Seconds -> Space -> ControlData -> Ptr QPainter -> Scene Object_ -> IO (Scene Object_)
stepScene now space controlData ptr =
    fromPure (updateNow now) >>>>
    updateContacts >>>>
    updateScene controlData >>>>
    passThrough (stepSpace space) >>>>
    renderScene ptr now >>>>
    fromPure (maybeId (flip transition controlData))

-- * step time measurement

-- | updates the fields now and oldNow
updateNow :: Seconds -> Scene o -> Scene o
updateNow now' scene@Scene{now} =
    scene{now = now', oldNow = now}

-- | updates the existing contacts
updateContacts :: Scene o -> IO (Scene o)
updateContacts scene = do
    contacts' <- peekContacts $ contactRef scene
    return $ scene{contacts = contacts'}


-- * State automaton stuff

transition :: Scene Object_ -> ControlData -> Maybe (Scene Object_)
transition scene (ControlData pushed _) = runHandler [
    nikkiToTerminal scene pushed,
    terminalExit scene,
    robotToTerminal scene pushed,
    gameOver scene,
    levelPassed scene
  ]

runHandler :: Sort s o => [Maybe (Scene o)] -> Maybe (Scene o)
runHandler (Just scene : _) = Just $ sendStartControl scene
  where
    sendStartControl :: Sort s o => Scene o -> Scene o
    sendStartControl scene = 
        modifyMainlayerObjectByIndex startControl (getControlledIndex scene) scene
runHandler (Nothing : r) = runHandler r
runHandler [] = Nothing


-- | converts the Scene to TerminalMode, if appropriate
nikkiToTerminal :: Scene Object_ -> [AppEvent] -> Maybe (Scene Object_)
nikkiToTerminal scene@Scene{mode = (NikkiMode nikki)} pushed
    | bPressed && beforeTerminal
        = Just $ scene {mode = mode'}
  where
    bPressed = Press BButton `elem` pushed
    beforeTerminal = nikkiTouchesTerminal $ contacts scene

    mode' = TerminalMode nikki terminal
    terminal = whichTerminalCollides scene
nikkiToTerminal _ _ = Nothing


whichTerminalCollides :: Scene Object_ -> Index
whichTerminalCollides Scene{objects, contacts} =
    case findIndices p allTerminals of
        (a : _) -> a
  where
    allTerminals :: Indexable (Maybe Terminal)
    allTerminals = fmap unwrapTerminal $ content $ mainLayer objects

    p :: Maybe Terminal -> Bool
    p Nothing = False
    p (Just t) = any (\ shape -> hasTerminalShape t shape) collidingShapes
    collidingShapes :: [Shape]
    collidingShapes = contacts |> terminals |> Set.toList

terminalExit :: Scene Object_ -> Maybe (Scene Object_)
terminalExit scene@Scene{mode = TerminalMode{nikki, terminal}} =
    case unwrapTerminal $ getMainlayerObject scene terminal of
        Just t -> case exitMode t of
            DontExit -> Nothing
            ExitToNikki ->
                Just scene{mode = NikkiMode nikki}
            ExitToRobot robot ->
                Just scene{mode = RobotMode nikki terminal robot}
terminalExit _ = Nothing


-- | converts from RobotMode to TerminalMode, if appropriate
robotToTerminal :: Scene Object_ -> [AppEvent] -> Maybe (Scene Object_)
robotToTerminal scene@Scene{mode = RobotMode{nikki, terminal}} pushed
  | bPress =
    Just $ scene{mode = TerminalMode nikki terminal}
  where
    bPress = Press BButton `elem` pushed
robotToTerminal _ _ = Nothing

gameOver :: Scene Object_ -> Maybe (Scene Object_)
gameOver scene | nikkiTouchesLaser $ contacts scene =
    Just $ modifyMode (const $ LevelFinished Failed) scene
gameOver _ = Nothing

levelPassed :: Scene Object_ -> Maybe (Scene Object_)
levelPassed scene | nikkiTouchesMilkMachine $ contacts scene =
    Just $ modifyMode (const $ LevelFinished Passed) scene
levelPassed _ = Nothing


-- * chipmunk stepping

stepSpace :: Space -> Scene Object_ -> IO ()

stepSpace _space Scene{mode = TerminalMode{}} = return ()

stepSpace space s@Scene{now, oldNow} = do
    forM_ [0..n] $ const $
        CM.step space stepQuantum
  where
    n :: Int = stepQuantums now - stepQuantums oldNow
    stepQuantums :: Seconds -> Int
    stepQuantums t = truncate (t / stepQuantum)

singleStepTime :: Fractional n => n
singleStepTime = 0.1

-- * object updating

-- | updates every object
updateScene :: ControlData -> Scene Object_ -> IO (Scene Object_)
updateScene cd scene@Scene{now, objects, contacts, mode} = do
    backgrounds' <- fmapM (modifyContentM (fmapMWithIndex updateMultiLayerObjects)) backgrounds
    (sceneChange, mainLayer') <- updateMainLayer mainLayer
    foregrounds' <- fmapM (modifyContentM (fmapMWithIndex updateMultiLayerObjects)) foregrounds
    return $ sceneChange $ scene{objects = Grounds backgrounds' mainLayer' foregrounds'}
  where
    controlled = getControlledIndex scene
    (Grounds backgrounds mainLayer foregrounds) = objects

    -- update function for all objects in the mainLayer
    updateMainLayer :: Layer Object_ -> IO (Scene Object_ -> Scene Object_, Layer Object_)
    -- each object has to know, if it's controlled
    updateMainLayer layer@Layer{content = ix} = do
        ix' <- fmapMWithIndex (\ i o ->
                update o i now contacts (i == controlled, cd)) ix
        let changes = foldr (.) id $ fmap fst ix'
            ix'' = fmap snd ix'
        return $ (changes, layer{content = ix''})

    -- update function for updates outside the mainLayer
    -- NOTE: SceneChanges currently only affect the main layer
    updateMultiLayerObjects :: Index -> Object_ -> IO Object_
    updateMultiLayerObjects i o = update o i now contacts (False, cd) >>= fromPure snd


-- * rendering

-- | well, renders the scene to the screen (to the max :)
renderScene :: Ptr QPainter -> Seconds -> Scene Object_ -> IO (Scene Object_)
renderScene ptr now scene@Scene{} = do

    resetMatrix ptr
    windowSize <- sizeQPainter ptr
    eraseRect ptr zero windowSize (QtColor 0 0 0 255)

    controlledPosition <- getPosition $ body $ chipmunk $ getControlled scene
    (center, cameraState') <- getCenter controlledPosition (cameraState scene)

    intSize@(Size width height) <- sizeQPainter ptr
    let size = fmap fromIntegral intSize
        offsetVector = - (center - Vector (fromIntegral width / 2) (fromIntegral height / 2))
        offset = fmap (fromIntegral . truncate) $ vector2QtPosition offsetVector

    when (showScene Configuration.development) $ do
        let os = objects scene
        fmapM_ (renderLayer ptr size offset now) $ backgrounds os
        renderLayer ptr size offset now $ mainLayer os
        fmapM_ (renderLayer ptr size offset now) $ foregrounds os


-- debugging
    when (showXYCross Configuration.development) $
        debugDrawCoordinateSystem ptr offset
    when (showChipmunkObjects Configuration.development) $
        fmapM_ (renderObjectGrid ptr offset) $ mainLayer $ objects scene



    return scene{cameraState = cameraState'}


-- | renders the different Layers.
-- makes sure, everything is rendered ok.
renderLayer :: Ptr QPainter -> Size Double -> Offset Double -> Seconds
    -> Layer Object_ -> IO ()
renderLayer ptr size offset now layer = do
    let modifiedOffset = calculateLayerOffset size offset layer
    fmapM_ (\ o -> render_ o ptr modifiedOffset now) (content layer)


-- * debugging

debugDrawCoordinateSystem :: Ptr QPainter -> Offset Double -> IO ()
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
    let chip = chipmunk object
    renderGrid ptr chip

-- renderObjectGrid ptr offset o = es "renderGrid" o

