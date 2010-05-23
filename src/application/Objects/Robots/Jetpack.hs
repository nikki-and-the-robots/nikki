{-# language NamedFieldPuns, ViewPatterns #-}

module Objects.Robots.Jetpack (initialState, jetpackRobotHandler) where


import Prelude hiding (lookup)

import Data.Abelian
import Data.Maybe
import Data.Directions

import Control.Monad.Compose

import Graphics.Qt as Qt

import Physics.Chipmunk as CM


import Utils
import Base.Constants

import Base.Events
import Base.Sprited

import Game.Scene.Types

import Objects.Collisions
import Objects.Animation
import Objects.Types
import Objects.Robots.Types
import Objects.Robots.Handler


-- jetpack robot

jetpackRobotHandler :: RobotHandler
jetpackRobotHandler = RobotHandler
    Objects.Robots.Jetpack.initialisation
    id
    Objects.Robots.Jetpack.update
    Objects.Robots.Jetpack.rendering

initialisation :: UninitializedScene -> Space -> UninitializedObject -> IO Object
initialisation _ space robot@(Robot s p typ) = do
        let size = defaultPixmapSize s
            bodyAttributes = bodyAttributesConstant{position = p}
            shapeAttributes = ShapeAttributes{
                elasticity = 0.8,
                friction = 0.5,
                collisionType = toCollisionType robot
              }
            (polys, baryCenterOffset) = mkPolys size
            shapesAndPolys = map (tuple shapeAttributes) polys

        chip <- initChipmunk space bodyAttributes shapesAndPolys baryCenterOffset
        return $ Robot s chip typ

bodyAttributesConstant :: BodyAttributes
bodyAttributesConstant = BodyAttributes {
    position = zero,
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





initialState :: RobotState
initialState = JetpackState False Nothing initialAnimation

initialAnimation :: Animation
initialAnimation = mkAnimation (UndirectedFrameSetType Idle) inner 0
  where
    inner :: FrameSetType -> AnimationPhases
    inner (frameSetAction -> Idle) = AnimationPhases $ zip
        (cycle [0 .. 3]) -- frame numbers
        (repeat robotIdleEyeTime) -- seconds per frame
        -- repeat x == cycle [x]
    inner (frameSetAction -> Boost) = AnimationPhases $ zip
        (cycle [0, 1])
        (repeat 0.08)
    inner (frameSetAction -> Wait) = robotWaitAnimation
    inner x = es "inner (Anim)" x


update :: Scene -> Seconds -> Collisions -> (Bool, ControlData) -> Object -> IO Object
update _ now _ (isControlled, cd) =
    pure (updateLogick (isControlled, cd)) >=>
    pure (modifyRobotState (updateAnimationState now isControlled)) >=>
    controlToChipmunk

-- * logick

updateLogick :: (Bool, ControlData) -> Object -> Object
updateLogick cd r@Robot{robotState} =
    r{robotState = updateState cd robotState}

updateState :: (Bool, ControlData) -> RobotState -> RobotState
updateState (False, _) JetpackState{robotAnimation} =
    JetpackState False Nothing robotAnimation
updateState (True, (ControlData _ held)) (JetpackState _ _ animation) =
    JetpackState boost direction animation
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

-- * Animation update

updateAnimationState :: Seconds -> Bool -> RobotState -> RobotState
updateAnimationState seconds isControlled (JetpackState boost direction animation) =
    JetpackState boost direction animation'
  where
    idle = not isControlled
    animationType' = UndirectedFrameSetType $
        if idle then
            Idle
          else
            if boost then Boost else Wait
    animation' = updateAnimation seconds animationType' animation


-- * chipmunk control

controlToChipmunk :: Object -> IO Object
controlToChipmunk object@(Robot _ Chipmunk{body} state) = do
    angle <- normalizeAngle body
    angVel <- getAngVel body

    -- hovering
    hover body angle (robotBoost state)

    -- angle correction
--     correctAngle (robotBoost state) body angle angVel

    -- directions
    let controlTorque = directions $ robotDirection state
        frictionTorque_ = frictionTorque angVel
        uprightCorrection_ = uprightCorrection state angle

        appliedTorque = frictionTorque_ +~ controlTorque +~ uprightCorrection_

    setTorque body (appliedTorque <<| "appliedTorque")

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

uprightCorrection state angle =
    if robotBoost state && isNothing (robotDirection state) then
        - (signum angle) * directionForce * 0.6
      else
        zero

-- correctAngle :: Bool -> Body -> Angle -> AngVel -> IO ()
-- correctAngle boost body angle angVel = do
--     let velocityCorrection = - (sqrt (abs angVel) * signum angVel) * (correctionFactor * 2.3)
--         angleCorrection = if boost then - angle * correctionFactor else 0
--         correctionFactor = 70
--         torque = velocityCorrection + angleCorrection
--         maxTorque = 300
-- --     when (abs angle > epsilon) $
--     setTorque body (clip (- maxTorque, maxTorque) (torque <<? "torque"))


-- * Rendering

rendering :: Ptr QPainter -> Scene -> Qt.Position Double -> Object -> IO ()
rendering ptr _ offset (Robot sprited chipmunk (JetpackState boost direction animation)) = do
    let pixmap = animationPixmap animation sprited
    renderChipmunk ptr offset pixmap chipmunk



