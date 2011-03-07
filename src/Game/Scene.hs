{-# language NamedFieldPuns, ScopedTypeVariables, ViewPatterns #-}

module Game.Scene (
    Scene,
    Game.Scene.immutableCopy,
    stepScene,
    renderScene,
  ) where

import Prelude hiding (foldr)

import Data.Indexable (Indexable, Index, findIndices, fmapMWithIndex, toList)
import Data.Map ((!))
import qualified Data.Set as Set
import Data.Foldable (foldr)
import Data.Maybe
import Data.Abelian

import Control.Monad
import Control.Monad.State (StateT(..))
import Control.Applicative ((<|>))

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base

import Object

import Game.Scene.Camera

import Sorts.Nikki.Types
import Sorts.Terminal
import Sorts.Switch


-- * entry

-- order of one frame step is this:
-- maybe transition to another scene
-- chipmunk stepping
-- updating the objects
-- rendering

stepScene :: Space -> ControlData -> Scene Object_ -> IO (Scene Object_)
stepScene space controlData =
    updateScene controlData >>>>
    stepSpace space >>>>
    transition controlData


-- * State automaton stuff

transition :: ControlData -> Scene Object_ -> IO (Scene Object_)
transition (pressed -> pressed) scene =
    case mNew of
        Nothing -> return scene
        Just new -> modifyTransitioned new
  where
    -- | Maybe the new scene
    mNew :: Maybe (Scene Object_)
    mNew = foldl1 (<|>) [
        nikkiToTerminal scene pressed,
        terminalExit scene,
        robotToTerminal scene pressed,
        nikkiMovedAwayFromTerminal scene,
        gameOver scene,
        levelPassed scene
      ]

-- | applied to the scene after every transition
modifyTransitioned :: Scene Object_ -> IO (Scene Object_)
modifyTransitioned scene = do
    resetHeldKeys
    return $ case getControlledIndex scene of
      Just controlledIndex ->
        let now = spaceTime scene
        in modifyMainlayerObjectByIndex (startControl now) controlledIndex scene
      Nothing -> scene


-- | converts the Scene to TerminalMode, if appropriate
nikkiToTerminal :: Scene Object_ -> [Button] -> Maybe (Scene Object_)
nikkiToTerminal scene@Scene{mode = (NikkiMode nikkiIndex)} pressed
                                               -- nikki must be in wait mode
    | bButtonPressed && beforeTerminal && waiting
        = Just $ scene {mode = mode'}
  where
    bButtonPressed = any isBButton pressed
    beforeTerminal = nikkiTouchesTerminal $ contacts scene
    nikki :: Nikki
    Just nikki = unwrapNikki $ getMainlayerObject scene nikkiIndex
    action_ = action $ state $ nikki
    waiting = isWaitAction action_

    mode' = TerminalMode nikkiIndex terminal
    (terminal : _) = whichTerminalCollides scene
nikkiToTerminal _ _ = Nothing


whichTerminalCollides :: Scene Object_ -> [Index]
whichTerminalCollides Scene{objects, contacts} =
    findIndices p allTerminals
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
        Just t -> case terminalExitMode t of
            DontExit -> Nothing
            ExitToNikki ->
                Just scene{mode = NikkiMode nikki}
            ExitToRobot robot ->
                Just scene{mode = RobotMode nikki terminal robot}
terminalExit _ = Nothing


-- | converts from RobotMode to TerminalMode, if appropriate
robotToTerminal :: Scene Object_ -> [Button] -> Maybe (Scene Object_)
robotToTerminal scene@Scene{mode = RobotMode{nikki, terminal}} pressed
  | bPress =
    Just $ scene{mode = TerminalMode nikki terminal}
  where
    bPress = any isBButton pressed
robotToTerminal _ _ = Nothing

-- | if nikki gets moved away from the terminal during robot mode...
nikkiMovedAwayFromTerminal :: Scene Object_ -> Maybe (Scene Object_)
nikkiMovedAwayFromTerminal scene@Scene{mode} =
    if isRobotMode mode || isTerminalMode mode then
        if terminal mode `elem` whichTerminalCollides scene then
            Nothing
        else
            Just $ scene{mode = NikkiMode (nikki mode)}
    else
        Nothing


gameOver :: Scene Object_ -> Maybe (Scene Object_)
gameOver scene | isGameOver =
    Just $ modifyMode (const $ LevelFinished now Failed) scene
  where
    now = spaceTime scene
    isGameOver =
        isGameMode (mode scene)
        && nikkiTouchesLaser (contacts scene)
gameOver _ = Nothing

levelPassed :: Scene Object_ -> Maybe (Scene Object_)
levelPassed scene =
    if allTriggered && isGameMode (mode scene) then
        Just $ modifyMode (const $ LevelFinished now Passed) scene
      else
        Nothing
  where
    allSwitches :: [Switch] = catMaybes $ map unwrapSwitch $ toList $ content $ mainLayer $ objects scene
    allTriggered = all triggered allSwitches
    now = spaceTime scene


-- * chipmunk stepping

-- | run steps in the physics simulation and update the contacts
-- leaves contacts untouched, if no simulation steps are being done

stepSpace :: Space -> Scene Object_ -> IO (Scene Object_)

stepSpace space s@Scene{contactRef} = do
    resetContactRef contactRef
    CM.step space stepQuantum
    contacts' <- readContactRef contactRef
    return s{contacts = contacts', spaceTime = spaceTime s + stepQuantum}


-- * object updating

-- | updates every object
updateScene :: ControlData -> Scene Object_ -> IO (Scene Object_)
updateScene cd scene@Scene{spaceTime = now, objects, contacts, mode} = do
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
                update DummySort mode now contacts (Just i == controlled, cd) i o) ix
        let changes = foldr (.) id $ fmap fst ix'
            ix'' = fmap snd ix'
        return $ (changes, layer{content = ix''})

    -- update function for updates outside the mainLayer
    -- NOTE: SceneChanges currently only affect the main layer
    updateMultiLayerObjects :: Index -> Object_ -> IO Object_
    updateMultiLayerObjects i o = update DummySort mode now contacts (False, cd) i o >>= fromPure snd


-- * rendering

-- | immutable copy (for the rendering thread)
immutableCopy :: Scene Object_ -> IO (Scene Object_)
immutableCopy = modifyObjectsM (modifyMainLayerM (fmapM Base.immutableCopy))


-- | well, renders the scene to the screen (to the max :)
renderScene :: Application -> Configuration -> Ptr QPainter
    -> Scene Object_ -> DebuggingCommand -> StateT CameraState IO ()
renderScene app configuration ptr scene@Scene{spaceTime = now, mode} debugging = do
    center <- getCameraPosition ptr scene
    io $ do
        size@(Size width height) <- fmap fromIntegral <$> sizeQPainter ptr
        let offsetVector = - (center - Vector (width / 2) (height / 2))
            offset = fmap (fromIntegral . truncate) $ vector2QtPosition offsetVector

        clearScreen ptr black

        when (not $ omit_pixmap_rendering configuration) $ do
            let os = objects scene
            fmapM_ (renderLayer ptr size offset now) $ backgrounds os
            renderLayer ptr size offset now $ mainLayer os
            fmapM_ (renderLayer ptr size offset now) $ foregrounds os

        renderTerminalOSD ptr now scene
        renderLevelFinishedOSD ptr app mode


        -- debugging
        when (render_xy_cross configuration) $
            debugDrawCoordinateSystem ptr offset
        when (render_chipmunk_objects configuration) $
            fmapM_ (renderObjectGrid ptr offset) $ mainLayer $ objects scene
        io $ debugging ptr offset


-- | renders the different Layers.
-- makes sure, everything is rendered ok.
renderLayer :: Ptr QPainter -> Size Double -> Offset Double -> Seconds
    -> Layer Object_ -> IO ()
renderLayer ptr size offset now layer = do
    let modifiedOffset = calculateLayerOffset size offset layer
    fmapM_ (\ o -> render_ o ptr modifiedOffset now) (content layer)

-- | renders the big osd images ("SUCCESS" or "FAILURE") at the end of levels
renderLevelFinishedOSD :: Ptr QPainter -> Application -> Mode -> IO ()
renderLevelFinishedOSD ptr app (LevelFinished _ result) = do
    resetMatrix ptr
    windowSize <- fmap fromIntegral <$> sizeQPainter ptr
    let pixmap = finished (applicationPixmaps app) ! result
        osdSize = pixmapSize pixmap -~ Size (fromUber 1) (fromUber 1)
                                    -- because they have a shadow of one uberpixel
        position = fmap (fromIntegral . round . (/ 2)) $ sizeToPosition (windowSize -~ osdSize)
    translate ptr position
    renderPixmapSimple ptr pixmap
renderLevelFinishedOSD ptr _ _ = return ()


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

-- renderObjectGrid ptr offset o = es "renderGrid" o

