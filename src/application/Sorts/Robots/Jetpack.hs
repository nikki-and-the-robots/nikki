{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
     FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK ignore-exports #-}


module Sorts.Robots.Jetpack (sorts) where


import Data.Abelian
import Data.Maybe
import Data.Generics
import Data.Map hiding (size, map)

import System.FilePath

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base.Directions
import Base.Constants
import Base.Events
import Base.Animation
import Base.Pixmap

import Object


-- * Configuration

animationFrameTimesMap :: Map RenderState [Seconds]
animationFrameTimesMap = fromList [
    (Idle, [robotIdleEyeTime]),
    (Wait, [3, 0.15, 0.1, 0.15]),
    (Boost, [0.08])
  ]


-- * loading

sorts :: IO [Sort_]
sorts = do
    pixmaps <- fmapM (fmapM (loadPixmap 1 . mkJetpackPngPath)) pngMap
    let r = JSort pixmaps
    return $ map Sort_ [r]

pngMap :: Map RenderState [String]
pngMap = fromList [
    (Wait, ["wait_00", "wait_01"]),
    (Boost, ["boost_00", "boost_01"]),
    (Idle, ["idle_00", "idle_01", "idle_02", "idle_03"])
  ]

mkJetpackPngPath name = pngDir </> "robots" </> "jetpack" </> name <.> "png"

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
    jchipmunk :: Chipmunk,
    boost :: Bool,
    direction :: Maybe HorizontalDirection,
    renderState :: RenderState,
    startTime :: Seconds
  }
    deriving (Show, Typeable)

instance Sort JSort Jetpack where
    sortId = const $ SortId "robots/jetpack"
    size = pixmapSize . defaultPixmap . pixmaps
    sortRender sort =
        sortRenderSinglePixmap (defaultPixmap $ pixmaps sort) sort

    initialize sort (Just space) ep Nothing = do
        let 
            pos = qtPosition2Vector (editorPosition2QtPosition sort ep)
                    +~ baryCenterOffset
            bodyAttributes = bodyAttributesConstant{CM.position = pos}
            shapeAttributes = ShapeAttributes{
                elasticity = 0.8,
                friction = 0.5,
                CM.collisionType = RobotCT
              }
            (polys, baryCenterOffset) = mkPolys $ size sort
            shapesAndPolys = map (tuple shapeAttributes) polys

        chip <- initChipmunk space bodyAttributes shapesAndPolys baryCenterOffset
        return $ Jetpack chip False Nothing Idle 0

    chipmunk = jchipmunk

    updateNoSceneChange object now contacts (isControlled, cd) = inner object
      where
        inner =
            fromPure (jupdate (isControlled, cd)) >>>>
            fromPure (updateRenderState now isControlled) >>>>
            controlToChipmunk

    render = renderJetpack





bodyAttributesConstant :: BodyAttributes
bodyAttributesConstant = BodyAttributes {
    CM.position = zero,
    mass = 50,
    inertia = 6000
  }


mkPolys :: Size Double -> ([ShapeType], Vector)
mkPolys (Size w h) =
     (rects, baryCenterOffset)
  where
    rects = map (mapVectors (-~ baryCenterOffset)) [
        bodyRect,
        legsRect,
        leftEngine,
        rightEngine
      ]
    bodyRect = mkRect (Position 32 0) (Size 60 68)
    legsRect = mkRect (Position 36 68) (Size 52 16)
    leftEngine = mkRect (Position 8 20) engineSize
    rightEngine = mkRect (Position 92 20) engineSize

    engineSize = Size 24 48

    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh


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

    aButton = AButton `elem` held
    left = LeftButton `elem` held
    right = RightButton `elem` held


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
    renderChipmunk ptr offset pixmap (jchipmunk j)

pickPixmap :: Seconds -> Jetpack -> JSort -> Pixmap
pickPixmap now j sort =
    pickAnimationFrame pixmapList animationFrameTimes (now - startTime j)
  where
    pixmapList = pixmaps sort ! renderState j
    animationFrameTimes = animationFrameTimesMap ! renderState j


-- * chipmunk control

controlToChipmunk :: Jetpack -> IO Jetpack
controlToChipmunk object@Jetpack{jchipmunk, boost, direction} = do
    angle <- normalizeAngle $ body jchipmunk
    angVel <- getAngVel $ body jchipmunk

    -- hovering
    hover (body jchipmunk) angle boost

    -- angle correction
--     correctAngle (robotBoost state) body angle angVel

    -- directions
    let controlTorque = directions direction
        frictionTorque_ = frictionTorque angVel
        uprightCorrection_ = uprightCorrection boost direction angle

        appliedTorque = frictionTorque_ +~ controlTorque +~ uprightCorrection_

    setTorque (body jchipmunk) (appliedTorque <<| "appliedTorque")

    return object

hover :: Body -> Angle -> Bool -> IO ()
hover body angle boost =
    if boost then
        applyOnlyForce body (jetpackForce <<| "jetpackForce" +~ antiGravity) zero
      else
        applyOnlyForce body (antiGravity <<| "antiGravity") zero
  where
    antiGravity = Vector 0 (antiGravityFactor * (- gravity) * mass bodyAttributesConstant)
    antiGravityFactor = 0.8

    jetpackForce = rotateVector angle (Vector 0 (- jetpackAmount))
    jetpackAmount = acceleration * mass bodyAttributesConstant
    acceleration = gravity - (antiGravityFactor * gravity) + 300

directions :: Maybe HorizontalDirection -> Torque
directions Nothing = 0
directions (Just HLeft) = (- directionForce)
directions (Just HRight) = directionForce

directionForce :: Torque
directionForce = (7.5 * inertia bodyAttributesConstant) <<| "directionForce"

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
