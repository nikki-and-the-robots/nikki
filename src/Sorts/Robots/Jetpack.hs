{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
     FlexibleInstances, DeriveDataTypeable #-}

module Sorts.Robots.Jetpack (sorts) where


import Data.Abelian
import Data.Maybe
import Data.Generics
import Data.Map hiding (size, map, member)
import Data.Set (member)
import Data.Directions

import System.FilePath

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base

import Object

import Sorts.Robots.Configuration


-- * Configuration

animationFrameTimesMap :: Map RenderState [Seconds]
animationFrameTimesMap = fromList [
    (Idle, [robotIdleEyeTime]),
    (Wait, [3, 0.15, 0.1, 0.15]),
    (Boost, [0.08])
  ]

jetpackSize = fmap fromUber $ Size 27 21


-- * loading

sorts :: RM [Sort_]
sorts = do
    pixmaps <- fmapM (fmapM (
                    fromPure mkJetpackPngPath >>>> 
                    getDataFileName >>>>
                    (io . loadJetpackPixmap)
                )) pngMap
    let r = JSort pixmaps
    return $ map Sort_ [r]

pngMap :: Map RenderState [String]
pngMap = fromList [
    (Wait, ["wait_00", "wait_01"]),
    (Boost, ["boost_00", "boost_01"]),
    (Idle, ["idle_00", "idle_01", "idle_02", "idle_03"])
  ]

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

data JSort = JSort {
    pixmaps :: Map RenderState [Pixmap]
  }
    deriving (Show, Typeable)

defaultPixmap :: Map RenderState [Pixmap] -> Pixmap
defaultPixmap m = head (m ! Wait)

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
    sortRender sort ptr _ =
        renderPixmapSimple ptr (defaultPixmap $ pixmaps sort)

    initialize sort (Just space) ep Nothing = do
        let
            pos = qtPosition2Vector (editorPosition2QtPosition sort ep)
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
        fromPure (jupdate (isControlled, cd)) >>>>
        fromPure (updateRenderState now isControlled) >>>>
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
    Size w h = jetpackSize


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

    aButton = AButton `member` held
    left = LeftButton `member` held
    right = RightButton `member` held


updateRenderState :: Seconds -> Bool -> Jetpack -> Jetpack
updateRenderState now controlled j =
    if renderState j /= newRenderState then
        j{
            renderState = newRenderState,
            startTime = 0
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
    let pixmap = pickPixmap now j sort
    renderChipmunk ptr offset pixmap (chipmunk j)

pickPixmap :: Seconds -> Jetpack -> JSort -> Pixmap
pickPixmap now j sort =
    pickAnimationFrame pixmapList animationFrameTimes (now - startTime j)
  where
    pixmapList = pixmaps sort ! renderState j
    animationFrameTimes = animationFrameTimesMap ! renderState j


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
