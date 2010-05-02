{-# language NamedFieldPuns #-}

module Physics.Chipmunk (
    Chipmunk(..),
    BodyAttributes(..),
    ShapeAttributes(..),

    getRenderPosition,
    getChipmunkPosition,

    initSpace,
    Physics.Chipmunk.initChipmunk,
    initStaticChipmunk,
    addInitShape,

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

    debugChipmunk,

    -- re-exports from submodules
    translateVector,
    renderGrid,
    rotateVector,
    positionToVector,
    vectorToPosition,
    mapVectors,
    vmap,
    mkRect,

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

    -- collisions
    MyCollisionType(..),
    addMyCallback,
    Callback(..),

  ) where


import Utils
import Constants

import Data.Abelian

import Control.Applicative ((<$>))

import Physics.Hipmunk

import Physics.Chipmunk.Types
import Physics.Chipmunk.CollisionCallbacks
import Physics.Chipmunk.DebugGrid




-- * Initial values

initSpace :: IO Space
initSpace = do
    Physics.Hipmunk.initChipmunk
    space <- newSpace

    -- space constants

    -- Number of iterations to use in the impulse solver to solve contacts.
    setIterations space 80 -- default: 10

    -- Number of iterations to use in the impulse solver to solve elastic collisions.
    setElasticIterations space 40 -- default: 0

    -- Default gravity to supply when integrating rigid body motions.
    setGravity space (Vector 0 gravity)

    -- Default damping to supply when integrating rigid body motions.
    setDamping space 1 -- default: 1

    -- global constants

    -- Number of frames that contact information should persist.
    setContactPersistence 0 -- default: 3 (number of frames)

    -- Amount of allowed penetration. Used to reduce vibrating contacts.
    setCollisionSlop 0.1 -- default: 0.1

    -- Determines how fast penetrations resolve themselves.
    let bc = 0.1
    setBiasCoef bc -- default: 0.1
    setConstraintBiasCoef bc -- default: 0.1

    return space

--     todo : freeSpace


initStaticChipmunk :: Space -> BodyAttributes -> [(ShapeAttributes, ShapeType)] -> Vector
    -> IO Chipmunk
initStaticChipmunk space as@StaticBodyAttributes{position = positionWithPadding} shapeTypes baryCenterOffset = do
    let normalAttrs = static2normalAttributes as
        position = positionWithPadding - Vector 1 1
    body <- mkBody normalAttrs baryCenterOffset
    let chip = StaticChipmunk space body [] position baryCenterOffset
    fst <$> addInitShape chip shapeTypes
initStaticChipmunk space x y bc = nm "initStaticChipmunk" (x, y)


initChipmunk :: Space -> BodyAttributes -> [(ShapeAttributes, ShapeType)] -> Vector
    -> IO Chipmunk
initChipmunk space as@BodyAttributes{} shapeTypes baryCenterOffset = do
    body <- mkBody as baryCenterOffset
    spaceAdd space body
    let chip = Chipmunk space body [] [] baryCenterOffset
    fst <$> addInitShape chip shapeTypes


initChipmunk s x ss bco = nm "initChipmunk" x

-- | initially adds shapes to a Chipmunk
addInitShape :: Chipmunk -> [(ShapeAttributes, ShapeType)] -> IO (Chipmunk, [Shape])
addInitShape (Chipmunk space body shapes shapeTypes baryCenterOffset) newShapeTypes = do
    newShapes <- mapM (uncurry (mkShape body)) newShapeTypes
    mapM_ (spaceAdd space) newShapes

    let chip = Chipmunk
            space body
            (shapes ++ newShapes)
            (shapeTypes ++ (map snd newShapeTypes))
            baryCenterOffset
    return (chip, newShapes)

addInitShape (StaticChipmunk space body shapeTypes position baryCenterOffset) newShapeTypes = do
    newShapes <- mapM (uncurry (mkShape body)) newShapeTypes
    mapM_ (spaceAdd space . Static) newShapes
    let chip =
            StaticChipmunk space body (shapeTypes ++ map snd newShapeTypes) position baryCenterOffset
    return (chip, newShapes)


-- * helpers

vectorX :: Vector -> Double
vectorX (Vector x y) = x

vectorY :: Vector -> Double
vectorY (Vector x y) = y


mkBody :: BodyAttributes -> Vector -> IO Body
mkBody BodyAttributes{position, mass, inertia} baryCenterOffset = do
    body <- newBody mass inertia
    setPosition body (position + baryCenterOffset)
    return body

mkShape :: Body -> ShapeAttributes -> ShapeType -> IO Shape
mkShape body ShapeAttributes{elasticity, friction, collisionType} shapeType = do
    shape <- newShape body shapeType zero
    setElasticity shape elasticity
    setFriction shape friction
    setMyCollisionType shape collisionType
    return shape


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







