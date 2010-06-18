{-# language NamedFieldPuns #-}

module Physics.Chipmunk (
    Chipmunk(..),
    BodyAttributes(..),
    ShapeAttributes(..),

    getRenderPosition,
    getChipmunkPosition,

    initSpace,
    Physics.Chipmunk.Types.initChipmunk,
    initStaticChipmunk,
--     addInitShape,
    removeChipmunk,

    vectorX,
    vectorY,

    modifyVelocity,
    modifyApplyForce,
    modifyApplyOnlyForce,
    modifyApplyImpulse,
    modifySurfaceVelocity,
    modifyAngle,
    modifyAngVel,
    normalizeAngle,

    mkStandardPolys,
    debugChipmunk,

    -- re-exports from submodules
    translateVector,
    renderGrid,
    rotateVector,
    qtPosition2Vector,
    vector2QtPosition,
    mapVectors,
    vmap,
    mkRect,
    module Physics.Chipmunk.ContactRef,

    -- re-exports from Physics.Hipmunk
    infinity,

    Vector(..),
    len,
    normalize,
    scale,
    toAngle,

    Body,
    Shape,
    getBody,
    ShapeType(..),
    StaticShape(..),
    Space,
    step,
    spaceAdd,
    spaceRemove,

    -- physics (and modification thereof)
    Position,
    getPosition,
    Velocity,
    getVelocity,
    Force,
    getForce,
    applyImpulse,
    applyForce,
    applyOnlyForce,

    getSurfaceVel,

    Angle,
    getAngle,
    AngVel,
    getAngVel,
    Torque,
    getTorque,
    setTorque,

    setSurfaceVel,

  ) where


import Utils

import Data.Abelian

import Physics.Hipmunk

import Physics.Chipmunk.Types
import Physics.Chipmunk.DebugGrid
import Physics.Chipmunk.ContactRef




-- * Initial values

initSpace :: CpFloat -> IO Space
initSpace gravity = do
    Physics.Hipmunk.initChipmunk
    space <- newSpace

    -- space constants

    -- Number of iterations to use in the impulse solver to solve contacts.
    setIterations space 10 -- default: 10

    -- Number of iterations to use in the impulse solver to solve elastic collisions.
    setElasticIterations space 10 -- default: 0

    -- Default gravity to supply when integrating rigid body motions.
    setGravity space (Vector 0 gravity)

    -- Default damping to supply when integrating rigid body motions.
    setDamping space 1 -- default: 1

    -- global constants

    -- Number of frames that contact information should persist.
    setContactPersistence 3 -- default: 3 (number of frames)

    -- Amount of allowed penetration. Used to reduce vibrating contacts.
    setCollisionSlop 0.1 -- default: 0.1

    -- Determines how fast penetrations resolve themselves.
    let bc = 0.1
    setBiasCoef bc -- default: 0.1
    setConstraintBiasCoef bc -- default: 0.1

    return space

--     todo : freeSpace


-- * helpers

vectorX :: Vector -> Double
vectorX (Vector x y) = x

vectorY :: Vector -> Double
vectorY (Vector x y) = y


-- * Controls

-- look in Hipmunk doc (common?)
modifyVelocity :: Chipmunk -> (Vector -> Vector) -> IO ()
modifyVelocity Chipmunk{body} f = do
    v <- getVelocity body
    setVelocity body (f v)

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
modifySurfaceVelocity shape f = do
    sv <- getSurfaceVel shape
    setSurfaceVel shape (f sv)

modifyAngle :: Chipmunk -> (Angle -> Angle) -> IO ()
modifyAngle Chipmunk{body} f = do
    a <- getAngle body
    setAngle body (f a)

modifyAngVel :: Chipmunk -> (AngVel -> AngVel) -> IO ()
modifyAngVel Chipmunk{body} f = getAngVel body >>= setAngVel body . f


-- normalizes (and returns) the angle of an object to (-pi, pi)
normalizeAngle :: Body -> IO Angle
normalizeAngle body = do
    angle <- foldAngle <$> getAngle body
    setAngle body angle
    return angle

-- | folds the angle of a body to (- pi, pi)
foldAngle :: Double -> Double
foldAngle = foldToRange (- pi, pi)


-- * debugging

-- | prints human readable information about the current state of the object
debugChipmunk :: Chipmunk -> IO ()
debugChipmunk Chipmunk{body} = do
    putStrLn $ replicate 50 '='
    inner body getPosition "Position"
    inner body getVelocity "Velocity"
    inner body getForce    "Force   "
  where
    inner :: Show v => x -> (x -> IO v) -> String -> IO ()
    inner x getter name = do
        v <- getter x
        putStrLn (name ++ ": " ++ show v)







