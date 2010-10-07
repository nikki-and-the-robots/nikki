{-# language NamedFieldPuns #-}

module Physics.Chipmunk (
    Chipmunk(..),
    BodyAttributes(..),
    ShapeAttributes(..),
    ShapeDescription(ShapeDescription),
    mkShapeDescription,
    immutableCopy,

    getRenderPosition,
    getChipmunkPosition,

    initSpace,
    Physics.Chipmunk.Types.initChipmunk,
--     addInitShape,
    removeChipmunk,

    vectorX,
    vectorY,

    modifyPosition,
    modifyVelocity,
    modifyApplyForce,
    modifyApplyOnlyForce,
    modifyApplyImpulse,
    modifySurfaceVelocity,
    modifyAngle,
    modifyAngVel,
    normalizeAngle,
    foldAngle,

    mkStandardPolys,

    -- re-exports from submodules
    translateVector,
    renderGrids,
    rotateVector,
    qtPosition2Vector,
    vector2QtPosition,
    mapVectors,
    vmap,
    mkRect,
    mkRectFromPositions,
    module Physics.Chipmunk.ContactRef,

    -- re-exports from Physics.Hipmunk
    infinity,

    Vector(..),
    len,
    normalize,
    scale,
    toAngle,
    fromAngle,
    rotate,

    Body,
    Shape,
    ShapeType(..),
    StaticShape(..),
    Space,
    step,
    spaceAdd,
    spaceRemove,

    -- physics (and modification thereof)
    Position,
    Physics.Chipmunk.Types.getPosition,
    moment,

    Velocity,
    velocity,
    Force,
    applyImpulse,
    applyForce,
    applyOnlyForce,
    resetForces,

    surfaceVel,

    Angle,
    AngVel,
    angVel,
    Torque,
    torque,

    -- re-exports from ?Data.StateVar
    ($=),
    get,

  ) where


import Utils

import Data.Abelian
import Data.StateVar

import Physics.Hipmunk hiding (body)
import qualified Physics.Hipmunk as H

import Physics.Chipmunk.Types
import Physics.Chipmunk.DebugGrid
import Physics.Chipmunk.ContactRef




-- * Initial values

initSpace :: CpFloat -> IO Space
initSpace gravity = do
    H.initChipmunk
    space <- newSpace

    -- space constants

    -- Number of iterations to use in the impulse solver to solve contacts.
    iterations space $= 10 -- default: 10

    -- Default gravity to supply when integrating rigid body motions.
    H.gravity space $= (Vector 0 gravity)

    -- Default damping to supply when integrating rigid body motions.
    damping space $= 1 -- default: 1

    -- global constants

    -- Number of frames that contact information should persist.
    contactPersistence $= 1 -- default: 3 (number of frames)

    -- Amount of allowed penetration. Used to reduce vibrating contacts.
    collisionSlop $= 0.1 -- default: 0.1

    -- Determines how fast penetrations resolve themselves.
    let bc = 0.1
    biasCoef $= bc -- default: 0.1
    constraintBiasCoef $= bc -- default: 0.1

    return space

--     todo : freeSpace


-- * helpers

vectorX :: Vector -> Double
vectorX (Vector x y) = x

vectorY :: Vector -> Double
vectorY (Vector x y) = y


-- * Controls

-- look in Hipmunk doc (common?)

modifyPosition :: Chipmunk -> (Vector -> Vector) -> IO ()
modifyPosition Chipmunk{body} f =
    H.position body $~ f

modifyVelocity :: Chipmunk -> (Vector -> Vector) -> IO ()
modifyVelocity Chipmunk{body} f =
    velocity body $~ f

modifyApplyForce :: Chipmunk -> Vector -> IO ()
modifyApplyForce Chipmunk{body} v = do
    applyForce body v zero

modifyApplyOnlyForce :: Chipmunk -> Vector -> IO ()
modifyApplyOnlyForce Chipmunk{body} v =
    applyOnlyForce body v zero

modifyApplyImpulse :: Chipmunk -> Vector -> IO ()
modifyApplyImpulse Chipmunk{body} v = do
    applyImpulse body v zero


modifySurfaceVelocity :: Shape -> (Vector -> Vector) -> IO ()
modifySurfaceVelocity shape f =
    surfaceVel shape $~ f

modifyAngle :: Chipmunk -> (Angle -> Angle) -> IO ()
modifyAngle Chipmunk{body} f = do
    angle body $~ f

modifyAngVel :: Chipmunk -> (AngVel -> AngVel) -> IO ()
modifyAngVel Chipmunk{body} f =
    angVel body $~ f


-- normalizes (and returns) the angle of an object to (-pi, pi)
normalizeAngle :: Body -> IO Angle
normalizeAngle body = do
    a <- foldAngle <$> get (angle body)
    angle body $= a
    return a

-- | folds the angle of a body to (- pi, pi)
foldAngle :: Double -> Double
foldAngle = foldToRange (- pi, pi)








