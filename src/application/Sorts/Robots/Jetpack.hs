{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses #-}

module Sorts.Robots.Jetpack (sorts) where


-- import Prelude hiding (lookup)

import Data.Abelian
import Data.Maybe
import Data.Directions

import Control.Monad.Compose

import System.FilePath

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base.Constants
import Base.Events
-- import Base.Sprited
-- 
-- import Object.Animation
import Object.Types as OT
-- import Object.Robots.Types


sorts :: IO [RSort]
sorts = do
    pixmap <- newQPixmap $ mkRobotPng "robot-jetpack_wait_00"
    size <- sizeQPixmap pixmap
    let r = RSort pixmap size
    return [r]

mkRobotPng name = pngDir </> "robots" </> name <.> "png"

data RSort = RSort {
    pixmapS :: Ptr QPixmap,
    rsize :: Size Int
  }

data Jetpack = Jetpack {
    pixmap :: Ptr QPixmap,
    jchipmunk :: Chipmunk,
    boost :: Bool,
    direction :: Maybe HorizontalDirection
--         robotAnimation :: Animation
  }


instance Sort RSort Jetpack where
    sortId = const $ SortId "robots/jetpack"
    size = rsize
    collisionType = const RobotCT
    sortRender sort =
        sortRenderSinglePixmap (pixmapS sort) sort

    initialize sort space ep = do
        let 
            pos = qtPositionToVector (editorPosition2QtPosition sort ep)
                    +~ baryCenterOffset
            bodyAttributes = bodyAttributesConstant{CM.position = pos}
            shapeAttributes = ShapeAttributes{
                elasticity = 0.8,
                friction = 0.5,
                CM.collisionType = OT.collisionType sort
              }
            (polys, baryCenterOffset) = mkPolys $ fmap fromIntegral $ size sort
            shapesAndPolys = map (tuple shapeAttributes) polys

        chip <- initChipmunk space bodyAttributes shapesAndPolys baryCenterOffset
        return $ Jetpack (pixmapS sort) chip False Nothing

    chipmunk = jchipmunk

    update object now _ (isControlled, cd) = inner object
      where
        inner =
            pure (jupdate (isControlled, cd)) >=>
    --         pure (modifyRobotState (updateAnimationState now isControlled)) >=>
            controlToChipmunk

    render jetpack ptr offset = do
--         let pixmap = animationPixmap animation sprited
        renderChipmunk ptr offset (pixmap jetpack) (jchipmunk jetpack)





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





-- initialState :: RobotState
-- initialState = JetpackState False Nothing initialAnimation
-- 
-- initialAnimation :: Animation
-- initialAnimation = mkAnimation (UndirectedFrameSetType Idle) inner 0
--   where
--     inner :: FrameSetType -> AnimationPhases
--     inner (frameSetAction -> Idle) = AnimationPhases $ zip
--         (cycle [0 .. 3]) -- frame numbers
--         (repeat robotIdleEyeTime) -- seconds per frame
--         -- repeat x == cycle [x]
--     inner (frameSetAction -> Boost) = AnimationPhases $ zip
--         (cycle [0, 1])
--         (repeat 0.08)
--     inner (frameSetAction -> Wait) = robotWaitAnimation
--     inner x = es "inner (Anim)" x



-- * logick

jupdate :: (Bool, ControlData) -> Jetpack -> Jetpack
jupdate (False, _) (Jetpack pixmap chip _ _)  =
    Jetpack pixmap chip False Nothing -- robotAnimation
-- updateState (True, (ControlData _ held)) (JetpackState _ _ animation) =
--     JetpackState boost direction animation
--   where
--     boost = aButton
--     direction =
--         if left then
--             if right then Nothing else Just HLeft
--           else if right then
--             Just HRight
--           else
--             Nothing
-- 
--     aButton = AButton `elem` held
--     left = LeftButton `elem` held
--     right = RightButton `elem` held
-- 
-- -- * Animation update
-- 
-- updateAnimationState :: Seconds -> Bool -> RobotState -> RobotState
-- updateAnimationState seconds isControlled (JetpackState boost direction animation) =
--     JetpackState boost direction animation'
--   where
--     idle = not isControlled
--     animationType' = UndirectedFrameSetType $
--         if idle then
--             Idle
--           else
--             if boost then Boost else Wait
--     animation' = updateAnimation seconds animationType' animation


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
