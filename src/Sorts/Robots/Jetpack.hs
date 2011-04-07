{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
     FlexibleInstances, DeriveDataTypeable #-}

module Sorts.Robots.Jetpack (sorts) where


import Data.Abelian
import Data.Maybe
import Data.Generics
import Data.Directions

import System.FilePath

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base

import Object

import Sorts.Robots.Configuration
import qualified Sorts.Robots.Eyes as Eyes


-- * Configuration

boostFrameTimes = [0.08]

jetpackSize :: Size Double
jetpackSize = fmap fromUber $ Size 27 21


-- * loading

sorts :: RM [Sort_]
sorts = do
    standardPixmap <- loadJetpackPng "standard_00"
    boostPixmaps <- mapM loadJetpackPng ["boost_00", "boost_01"]
    robotEyes <- Eyes.loadRobotEyesPixmaps
    return [Sort_ $ JSort standardPixmap boostPixmaps robotEyes]
  where
    loadJetpackPng :: String -> RM Pixmap
    loadJetpackPng =
        return . mkJetpackPngPath >=>
        getDataFileName >=>
        (io . loadJetpackPixmap)

mkJetpackPngPath name = pngDir </> "robots" </> "jetpack" </> name <.> "png"

-- | loads a pixmap with the special size and padding of jetpack pixmaps
loadJetpackPixmap :: FilePath -> IO Pixmap
loadJetpackPixmap file = do
    p <- newQPixmap file
    return (Pixmap p (Size (fromUber 27) (fromUber 21)) (Position (- 9) (- 1)))

data RenderState
    = Wait
    | Boost
    | Idle
  deriving (Eq, Ord, Show)

isBoost Boost = True
isBoost _ = False

data JSort = JSort {
    standardPixmap :: Pixmap,
    boostPixmaps :: [Pixmap],
    robotEyes :: Eyes.RobotEyesPixmaps
  }
    deriving (Show, Typeable)

data Jetpack = Jetpack {
    chipmunk :: Chipmunk,
    boost :: Bool,
    direction :: Maybe HorizontalDirection,
    renderState :: RenderState,
    startTime :: Seconds
  }
    deriving (Show, Typeable)

instance Sort JSort Jetpack where
    sortId = const $ SortId "robots/jetpack"
    size = const jetpackSize
    renderIconified sort ptr =
        renderPixmapSimple ptr (standardPixmap sort)

    initialize sort (Just space) ep Nothing = do
        let
            pos = position2vector (editorPosition2QtPosition sort ep)
                    +~ baryCenterOffset
            bodyAttributes = jetpackBodyAttributes pos
            (polys, baryCenterOffset) = mkPolys
            shapesAndPolys = map (mkShapeDescription robotShapeAttributes) polys

        chip <- initChipmunk space bodyAttributes shapesAndPolys baryCenterOffset
        return $ Jetpack chip False Nothing Idle 0

    chipmunks = chipmunk >>> return

    immutableCopy j@Jetpack{chipmunk} =
        CM.immutableCopy chipmunk >>= \ x -> return j{chipmunk = x}

    getControlledChipmunk _ = chipmunk

    updateNoSceneChange sort mode now contacts (isControlled, cd) =
        return . jupdate (isControlled, cd) >=>
        return . updateRenderState now isControlled >=>
        passThrough controlToChipmunk

    render = renderJetpack

jetpackBodyAttributes p =
    mkMaterialBodyAttributes robotMaterialMass (fst mkPolys) p

jetpackMass = mass $ jetpackBodyAttributes zero

jetpackInertia = inertia $ jetpackBodyAttributes zero


mkPolys :: ([ShapeType], Vector)
mkPolys =
    (rects, baryCenterOffset)
  where
    rects = map (mapVectors (-~ baryCenterOffset)) (
        bodyRect :
        legsRect :
        leftEngine :
        rightEngine :
        [])
    bodyRect = mkRect (fmap fromUber $ Position 6 0) robotBodySize
    legsRect = mkRect (fmap fromUber $ Position 7 15) (fmap fromUber $ Size 13 6)
    leftEngine = mkRect (fmap fromUber $ Position 0 5) engineSize
    rightEngine = mkRect (fmap fromUber $ Position 21 5) engineSize
    engineSize = fmap fromUber $ Size 6 12

    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh
    Size w h = fmap realToFrac $ jetpackSize


-- * logick

jupdate :: (Bool, ControlData) -> Jetpack -> Jetpack
jupdate (False, _) (Jetpack chip _ _ renderState times)  =
    Jetpack chip False Nothing renderState times
jupdate (True, (ControlData _ held)) (Jetpack chip _ _ renderState times) =
    Jetpack chip boost direction renderState times
  where
    boost = aButton
    direction =
        if left then
            if right then Nothing else Just HLeft
          else if right then
            Just HRight
          else
            Nothing

    aButton, left, right :: Bool
    aButton = fany isAButton held
    left = fany isLeft held
    right = fany isRight held


updateRenderState :: Seconds -> Bool -> Jetpack -> Jetpack
updateRenderState now controlled j =
    if renderState j /= newRenderState then
        j{
            renderState = newRenderState,
            startTime = now
          }
      else
        j
  where
    newRenderState =
        if not controlled then
            Idle
          else if boost j then
            Boost
          else
            Wait


renderJetpack j sort ptr offset now = do
    (pos, angle) <- getRenderPositionAndAngle $ chipmunk j
    let pixmap = pickPixmap now j sort
        background = RenderPixmap pixmap pos (Just angle)
        eyes = Eyes.renderRobotEyes (robotEyes sort) pos angle eyesOffset
            eyesState (now - startTime j)
    return (background : eyes : [])
  where
    eyesState = case renderState j of
        Idle -> Eyes.Idle
        Wait -> Eyes.Active
        Boost -> Eyes.Open

eyesOffset = fmap fromUber $ Position 8 3

pickPixmap :: Seconds -> Jetpack -> JSort -> Pixmap
pickPixmap now j sort =
    if isBoost (renderState j) then
        pickAnimationFrame (boostPixmaps sort) boostFrameTimes (now - startTime j)
      else
        standardPixmap sort


-- * chipmunk control

controlToChipmunk :: Jetpack -> IO ()
controlToChipmunk object@Jetpack{chipmunk, boost, direction} = do
    angle <- normalizeAngle $ body chipmunk
    angVel <- CM.get $ angVel $ body chipmunk

    -- hovering
    hover (body chipmunk) angle boost

    -- angle correction
--     correctAngle (robotBoost state) body angle angVel

    -- directions
    let controlTorque = directions direction
        frictionTorque_ = frictionTorque angVel
        uprightCorrection_ = uprightCorrection boost direction angle

        appliedTorque = frictionTorque_ +~ controlTorque +~ uprightCorrection_

    torque (body chipmunk) $= (appliedTorque <<| "appliedTorque")


hover :: Body -> Angle -> Bool -> IO ()
hover body angle boost =
    if boost then
        applyOnlyForce body (jetpackForce <<| "jetpackForce" +~ antiGravity) zero
      else
        applyOnlyForce body (antiGravity <<| "antiGravity") zero
  where
    antiGravity = Vector 0 (antiGravityFactor * (- gravity) * jetpackMass)
    antiGravityFactor = 0.8

    jetpackForce = rotateVector angle (Vector 0 (- jetpackAmount))
    jetpackAmount = acceleration * jetpackMass
    acceleration = gravity - (antiGravityFactor * gravity) + 300

directions :: Maybe HorizontalDirection -> Torque
directions Nothing = 0
directions (Just HLeft) = (- directionForce)
directions (Just HRight) = directionForce

directionForce :: Torque
directionForce = (7.5 * jetpackInertia) <<| "directionForce"

frictionTorque angVel =
    if abs angVel < epsilon then
        0
      else
        - (signum angVel * directionForce * 0.4)

uprightCorrection boost direction angle =
    if boost && isNothing direction then
        - (signum angle) * directionForce * 0.6
      else
        zero

-- -- correctAngle :: Bool -> Body -> Angle -> AngVel -> IO ()
-- -- correctAngle boost body angle angVel = do
-- --     let velocityCorrection = - (sqrt (abs angVel) * signum angVel) * (correctionFactor * 2.3)
-- --         angleCorrection = if boost then - angle * correctionFactor else 0
-- --         correctionFactor = 70
-- --         torque = velocityCorrection + angleCorrection
-- --         maxTorque = 300
-- -- --     when (abs angle > epsilon) $
-- --     setTorque body (clip (- maxTorque, maxTorque) (torque <<? "torque"))
