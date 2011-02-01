{-# language ViewPatterns, NamedFieldPuns, DeriveDataTypeable #-}

module Sorts.Robots.MovingPlatform.Path where


import Data.Abelian
import Data.Typeable

import Physics.Chipmunk

import Utils

import Sorts.Robots.MovingPlatform.Configuration


-- | Describes the path of a platform.
-- A platform path can be thought of as a cycle of nodes
-- (that make up a cycle of segments).
data Path = Path {
    nodes :: [Vector],
    lastNode :: Vector
  }
  deriving (Show, Typeable)

-- | returns the next path node
nextNode :: Path -> Vector
nextNode p@(nodes -> (a : _)) = a

-- | The platform has an active segment at any time,
-- between (lastNode platform) and (nextNode platform).
-- This operation switches to the next segment if needed.
-- If a switch takes place, an impulse is applied to
-- smoothen behaviour at path nodes.
updateSegment :: Chipmunk -> Path -> IO Path
updateSegment _ path@(Path [_] _) = return path
updateSegment chip path@(Path (next : r) lastNode) = do
    p <- getPosition chip
    let closestPathPoint = closestPointOnLineSegment (lastNode, next) p
    if closestPathPoint == next then do
        let newPath = Path (r +: next) next
        applyEdgeImpulse chip
                (foldAngle $ toAngle (next -~ lastNode))
                (foldAngle $ toAngle (nextNode newPath -~ next))
        return newPath
      else
        return path

-- | calculates the impulse to apply when switching path segments
applyEdgeImpulse :: Chipmunk -> Angle -> Angle -> IO ()
applyEdgeImpulse chip last next = do
    let b = body chip
    m <- getMass chip
    v <- get $ velocity b
    let delta = foldAngle (next - last)
        wantedVelocity = rotateVector delta v
        velocityDeviation = wantedVelocity -~ v
        impulse = scale velocityDeviation m
    applyImpulse b impulse zero


-- | (pure) calculation of the path force.
mkPathForce :: Path -> Double -> Vector -> Vector -> IO Vector
mkPathForce path m p v = do
--     mapM_ (uncurry $ debugLine lightGreen) $ adjacentCyclic $ nodes path
--     debugLine green (lastNode path) (nextNode path)
--     debugPoint white closestPathPoint
--     debugPoint yellow aim
--     debugPoint red $ guidePoint path

    return $

    -- the force will always have the same length (or 0)
        scale force forceLen
  where
    forceLen = m * platformAcceleration

    -- | point on the active segment that is closest to
    -- the platform's position
    closestPathPoint =
        closestPointOnLineSegment (lastNode path, nextNode path) p

    -- | point where the platform is headed.
    aim = addWeightedVectors
        (closestPathPoint, pathWeight)
        (nextNode path, (1 - pathWeight))
    -- | from the platform to the aim
    toAim = aim -~ p
    lenToAim = len toAim
    wantedVelocityLen :: Double
    wantedVelocityLen = mkWantedVelocityLen $ len (nextNode path -~ p)
    -- | this would be the ideal velocity for the platform's position
    -- relative to the aim
    wantedVelocity = scale (normalizeIfNotZero toAim) wantedVelocityLen
    -- | deviation between wantedVelocity and actual velocity
    velocityDeviation = wantedVelocity -~ v
    -- | normalized force to be applied
    force =
        if len velocityDeviation < velocityEpsilon then
            zero
          else
            normalizeIfNotZero velocityDeviation

-- | if the platform is closer to the next path node
-- than the decelerationDistance the platform should decelerate
-- at full force to reach minimumPlatformVelocity when reaching the
-- next path node.
decelerationDistance =
    abs( - (minimumPlatformVelocity ^ 2 - maximumPlatformVelocity ^ 2) /
         (2 * (- platformAcceleration)))

-- | The length of the wanted velocity.
mkWantedVelocityLen distance =
    if distance > decelerationDistance then
        maximumPlatformVelocity
      else
        -- deceleration
        slopeH * distance + minimumPlatformVelocity
slopeH =
    (maximumPlatformVelocity - minimumPlatformVelocity) /
    decelerationDistance

-- | adds two vectors with the given weights
addWeightedVectors :: (Vector, Double) -> (Vector, Double) -> Vector
addWeightedVectors (a, aw) (b, bw) =
    scale a aw +~
    scale b bw



-- * geometry

-- | calculates the closest point on a line segment to a given point.
closestPointOnLineSegment :: (Vector, Vector) -> Vector -> Vector
closestPointOnLineSegment (a, b) p =
    if a == b then a else
    if f <= 0 then a else if f >= 1 then b else
    a +~ scale (b -~ a) f
  where
    f = ((x p - x a) * (x b - x a) + (y p - y a) * (y b - y a)) /
        (len (b -~ a) ^ 2)
    x = vectorX
    y = vectorY

-- | mirrors an angle at a given angle
mirrorAngle :: Angle -> Angle -> Angle
mirrorAngle mirror angle =
    mirror - (angle - mirror)

