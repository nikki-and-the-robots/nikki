{-# language ViewPatterns, NamedFieldPuns, DeriveDataTypeable, ScopedTypeVariables #-}

module Sorts.Robots.MovingPlatform.Path where


import Data.Abelian
import Data.Typeable

import Physics.Chipmunk hiding (start, end)

import Graphics.Qt hiding (scale)

import Base

import Utils hiding (distance)

import Sorts.Robots.MovingPlatform.Configuration


-- | Describes the path of a platform.
-- A platform path can be thought of as a cycle of nodes
-- (that make up a cycle of segments).
data Path
    = Path {
        segments :: [Segment],
        distanceToGuidePoint :: Double,
        pathLength :: Double
      }
    | SingleNode {
        node :: Vector,
        onState :: Maybe Path -- saves the path if the platform can be switched on
      }
  deriving (Show, Typeable)

mkPath :: Bool -> [Vector] -> Path
mkPath _ [] = error "empty paths are not allowed"
mkPath _ [n] = SingleNode n Nothing
mkPath active list =
    (deleteConsecutiveTwins >>>
    adjacentCyclic >>>
    map (\ (a, b) -> segment a b) >>>
    (\ segments -> Path segments 0 (sumLength segments)) >>>
    wrap) list
  where
    -- deletes consecutive points in the path that are identical.
    deleteConsecutiveTwins :: Eq a => [a] -> [a]
    deleteConsecutiveTwins = mergeAdjacentCyclicPairs $
        \ a b -> if a == b then Just a else Nothing

    -- sums up all the segment's lengths
    sumLength :: [Segment] -> Double
    sumLength = sum . map segmentLength

    -- wraps the path in a SingleNode when platform is initially switched off
    wrap = if active then id else SingleNode (head list) . Just

data Segment = Segment {
    start :: Vector,
    end :: Vector,
    segmentLength :: Double
  }  deriving (Show, Typeable)

segment :: Vector -> Vector -> Segment
segment start end = Segment start end (len (end -~ start))

segmentToVector :: Segment -> Vector
segmentToVector segment = end segment -~ start segment

-- | returns the next path node
nextNode :: Path -> Vector
nextNode (segments -> (a : _)) = end a

lastNode :: Path -> Vector
lastNode (segments -> (a : _)) = start a


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
guidePoint path =
    inner (cycle $ segments path) (distanceToGuidePoint path)
  where
    inner (a : r) d =
        if d < segmentLength a then
            start a +~ scale (normalize $ segmentToVector a) d
          else
            inner r (d - segmentLength a)

-- | updates the guide with the configuration value for the platform speed
updateGuide :: Path -> Path
updateGuide p@SingleNode{} = p
updateGuide (Path segments@(segment : _) distance pathLength) =
    Path segments newDistance pathLength
  where
    tmpNewDistance = distance + stepQuantum * platformStandardVelocity
    newDistance =
        if tmpNewDistance > segmentLength segment then
            foldToRange
                (segmentLength segment,
                 segmentLength segment + pathLength)
                tmpNewDistance
          else
            tmpNewDistance

-- * segment switching

-- | The platform has an active segment at any time,
-- between (lastNode platform) and (nextNode platform).
-- This operation switches to the next segment if needed.
-- If a switch takes place, an impulse is applied to
-- smoothen behaviour at path nodes.
updateSegment :: Chipmunk -> Path -> IO Path
updateSegment _ p@SingleNode{} = return p
updateSegment chip path@(Path (a : r) dtg pathLength) = do
    p <- getPosition chip
    let last = lastNode path
        next = nextNode path
        closestPathPoint = closestPointOnLineSegment (last, next) p
        dtg' = (dtg - len (next -~ last))
    if closestPathPoint == next && dtg' >= 0 then do
        let newPath = Path (r +: a) dtg' pathLength
        applyNodeImpulse chip
                (foldAngle $ toAngle (next -~ last))
                (foldAngle $ toAngle (nextNode newPath -~ next))
        return newPath
      else
        return path

-- | calculates the impulse to apply when switching path segments
applyNodeImpulse :: Chipmunk -> Angle -> Angle -> IO ()
applyNodeImpulse chip last next = do
    let b = body chip
    m <- getMass chip
    v <- get $ velocity b
    let delta = foldAngle (next - last)
        wantedVelocity = rotateVector delta v
        velocityDeviation = wantedVelocity -~ v
        impulse = scale velocityDeviation (m * nodeImpulseFactor)
    applyImpulse b impulse zero


-- * force

-- | (pure) calculation of the path force.
mkPathForce :: Path -> Double -> Vector -> Vector -> Vector
mkPathForce (SingleNode aim _) m p v =
    force +~ drag
  where
    direction = normalizeIfNotZero (aim -~ p)
    force = scale direction forceLen
    forceLen = m * toAimLen * springFactor
    toAimLen = len (aim -~ p)
    -- the acceleration should increase with lenToAim
    -- till the springConstantAccelerationDistance is reached
    springFactor = platformAcceleration / springConstantAccelerationDistance
    -- drag to let the swinging stop
    drag = scale dragDirection dragLen
    dragLen = constantDrag +~ dynamicDrag
    constantDrag = frictionFactor * m * platformAcceleration
    dynamicDrag = dragFactor * m * platformAcceleration
        * len v / platformStandardVelocity
    dragDirection = normalizeIfNotZero (negateAbelian v)
mkPathForce path@Path{} m p v =
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
--         min decelerationRamp
        wantedVelocityLenWithoutDeceleration

    -- | Decelaration ramp. A value that will linearly approach platformMinimumVelocity
    -- when the next node is reached, dependent on the distance to that next node.
    -- Intersects with the platformStandardVelocity when the distance equals
    -- decelerationDistance.
    decelerationRamp :: Double
    decelerationRamp = decelerationRampFunction $ len (nextNode path -~ p)
    decelerationRampFunction x =
        m * x + c
      where
        m = (platformStandardVelocity - platformMinimumVelocity) / decelerationDistance
        c = platformMinimumVelocity

    -- | length of the wanted velocity without decelerating the platform
    wantedVelocityLenWithoutDeceleration =
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


-- | Distance to the next path node from which the platform will decelerate.
-- Lost time will be made up for by chasing a bit in the next poath segment.
decelerationDistance :: Double = fromKachel 0.5

-- | Velocity the platform will decelerate to on path nodes.
platformMinimumVelocity :: Double = 150



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
