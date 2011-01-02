{-# language NamedFieldPuns #-}

module Physics.Chipmunk (
    Chipmunk(..),
    BodyAttributes(..),
    ShapeAttributes(..),
    ShapeDescription(ShapeDescription, shapeType),
    mkShapeDescription,
    immutableCopy,

    getRenderPosition,
    getChipmunkPosition,

    withSpace,
    mkMaterialBodyAttributes,
    Physics.Chipmunk.Types.initChipmunk,
    removeChipmunk,

    vectorX,
    vectorY,
    modifyLen,

    modifyPosition,
    modifyVelocity,
    modifyApplyForce,
    modifyApplyOnlyForce,
    modifyApplyImpulse,
    modifySurfaceVelocity,
    modifyAngle,
    modifyAngVel,
    normalizeAngle,

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
    module Physics.Chipmunk.StickyEdges,

    foldAngle,
    toUpAngle,
    fromUpAngle,
    component,

    massForShape,
    momentForMaterialShape,

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
    Mass,
    applyImpulse,
    applyForce,
    applyOnlyForce,
    resetForces,

    surfaceVel,

    Angle,
    AngVel,
    angle,
    angVel,
    Torque,
    torque,

    -- re-exports from Data.StateVar
    ($=),
    get,

  ) where


import Data.Abelian
import Data.StateVar

import Control.Monad.CatchIO

import Physics.Hipmunk hiding (body)
import qualified Physics.Hipmunk as H

import Physics.Chipmunk.Types
import Physics.Chipmunk.DebugGrid
import Physics.Chipmunk.ContactRef
import Physics.Chipmunk.StickyEdges hiding (Rectangle(..))

import Utils


-- * Initial values

withSpace :: MonadCatchIO m => CpFloat -> (Space -> m a) -> m a
withSpace gravity cmd =
    bracket (io mkSpace) (io . freeSpace) cmd
  where
    mkSpace :: IO Space
    mkSpace = do
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
        contactPersistence $= 3 -- default: 3 (number of steps)

        -- Amount of allowed penetration. Used to reduce vibrating contacts.
        collisionSlop $= 0.1 -- default: 0.1

        -- Determines how fast penetrations resolve themselves.
        let bc = 0.1
        biasCoef $= bc -- default: 0.1
        constraintBiasCoef $= bc -- default: 0.1

        return space

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
