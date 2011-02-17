{-# language ViewPatterns, NamedFieldPuns, DeriveDataTypeable #-}

module Sorts.Robots.MovingPlatform.Path where


import Data.Abelian
import Data.Typeable

import Physics.Chipmunk

import Graphics.Qt hiding (scale)

import Base

import Utils

import Sorts.Robots.MovingPlatform.Configuration


-- | Describes the path of a platform.
-- A platform path can be thought of as a cycle of nodes
-- (that make up a cycle of segments).
data Path = Path {
    nodes :: [Vector],
    lastNode :: Vector,
    distanceToGuidePoint :: Double
  }
  deriving (Show, Typeable)

-- | returns the next path node
nextNode :: Path -> Vector
nextNode p@(nodes -> (a : _)) = a


updatePath :: Chipmunk -> Path -> IO Path
updatePath chip =
    fromPure updateGuide >>>>
    updateSegment chip


-- * guide point

-- The guide point is a point that moves on the path with a
-- constant velocity. It is guiding the movement of the platform.
-- It is described as the distance from (lastNode path) to the guide point
-- on the path.

-- | returns the guide point
-- (for debugging)
guidePoint :: Path -> Vector
guidePoint path | distanceToGuidePoint path < 0 =
    error "platform faster than guide point"
guidePoint (Path nodes lastNode distance) =
    inner (lastNode : cycle nodes) distance
  where
    inner (a : b : r) distance =
        if distance < lenSegment then
            a +~ scale (normalize segment) distance
          else
            inner (b : r) (distance - lenSegment)
      where
        segment = b -~ a
        lenSegment = len segment

-- | updates the guide with the configuration value for the platform speed
updateGuide :: Path -> Path
updateGuide (Path nodes last distance) =
    Path nodes last (distance + stepQuantum * platformStandardVelocity)


-- * segment switching

-- | The platform has an active segment at any time,
-- between (lastNode platform) and (nextNode platform).
-- This operation switches to the next segment if needed.
-- If a switch takes place, an impulse is applied to
-- smoothen behaviour at path nodes.
updateSegment :: Chipmunk -> Path -> IO Path
updateSegment _ path@(Path [_] _ _) = return path
updateSegment chip path@(Path (next : r) last dtg) = do
    p <- getPosition chip
    let closestPathPoint = closestPointOnLineSegment (last, next) p
        dtg' = (dtg - len (next -~ last))
    if closestPathPoint == next && dtg' >= 0 then do
        let newPath = Path (r +: next) next dtg'
        applyEdgeImpulse chip
                (foldAngle $ toAngle (next -~ last))
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


-- * force

-- | (pure) calculation of the path force.
mkPathForce :: Path -> Double -> Vector -> Vector -> IO Vector
mkPathForce path m p v = do
--     mapM_ (uncurry $ debugLine green) $ adjacentCyclic $ nodes path
--     debugLine lightGreen (lastNode path) (nextNode path)
--     debugPoint blue $ guidePoint path
--     debugPoint lightBlue segmentGuidePoint
--     debugPoint white closestPathPoint
--     debugPoint red aim
-- 
--     debugLine pink p (p +~ scale force 50)

    return $
    -- the force will always have the same length (or 0)
        scale force forceLen
  where
    forceLen = m * platformAcceleration
    -- | normalized force to be applied
    force =
        if len velocityDeviation < velocityEpsilon then
            zero
          else
            normalizeIfNotZero velocityDeviation

    -- | this would be the ideal velocity for the platform's position
    -- relative to the aim
    wantedVelocity = scale (normalizeIfNotZero toAim) wantedVelocityLen
    -- | deviation between wantedVelocity and actual velocity
    velocityDeviation = wantedVelocity -~ v

    -- | The length of the wanted velocity.
    wantedVelocityLen :: Double
    wantedVelocityLen =
        if len toAim < positionEpsilon then
            platformStandardVelocity
        else
            platformMaximumVelocity

    -- | guidePoint restricted to the actual segment.
    segmentGuidePoint =
        if distanceToGuidePoint path > len (nextNode path -~ lastNode path) then
            -- guidePoint is on the next segment
            nextNode path
          else
            guidePoint path

    -- | point on the active segment that is closest to
    -- the platform's position
    closestPathPoint =
        closestPointOnLineSegment (lastNode path, nextNode path) p

    -- | where the platform will go to
    aim = if len (segmentGuidePoint -~ closestPathPoint) < aimDistance then
            -- segmentGuidePoint is closer than aimDistance
            segmentGuidePoint
          else
            -- segmentGuidePoint is further away than aimDistance
            closestPathPoint +~ scale (normalize (segmentGuidePoint -~ closestPathPoint)) aimDistance
    toAim = aim -~ p


-- * geometry utils

-- | Calculates the closest point on a line segment to a given point.
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
