{-# language ViewPatterns, NamedFieldPuns, DeriveDataTypeable, ScopedTypeVariables #-}

module Sorts.Robots.MovingPlatform.Path where


import Data.Abelian
import Data.Typeable

import Physics.Chipmunk hiding (start, end)

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

-- | returns the currently active segment
currentSegment :: Path -> Segment
currentSegment Path{segments = (a : _)} = a

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


updatePath :: Path -> Path
updatePath =
    updateGuide >>>
    updateSegment


-- * guide point

-- The guide point is a point that moves on the path with a
-- constant velocity. It is guiding the movement of the platform.
-- It is described as the distance from (lastNode path) to the guide point
-- on the path.

-- | returns the guide point
guidePoint :: [Segment] -> Double -> Vector
guidePoint segments distanceToGuidePoint =
    inner (cycle segments) distanceToGuidePoint
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
updateSegment :: Path -> Path
updateSegment p@SingleNode{} = p
updateSegment path@(Path (a : r) dtg pathLength) =
    if dtg >= segmentLength a
    then Path (r +: a) (dtg - segmentLength a) pathLength
    else path


-- * force

-- | (pure) calculation of the path force.
mkPathForce :: Path -> Double -> Vector -> Vector -> Vector
mkPathForce (SingleNode aim _) m p v =
    springForce singleNodeSpringConfiguration m p v aim
mkPathForce (Path segments distanceToGuidePoint _) m p v = do
    -- the force will always have the same length (or 0)
    springForce pathSpringConfiguration m p v (guidePoint segments distanceToGuidePoint)


-- * spring simulation

-- | Simulates the attachment of the platforms to a spring.
springForce :: SpringConfiguration -> Mass -> Vector -> Vector -> Vector -> Vector
springForce conf mass position velocity aim =
    force +~ drag
  where
    direction = normalizeIfNotZero (aim -~ position)
    force = scale direction forceLen
    forceLen = mass * toAimLen * springFactor
    toAimLen = len (aim -~ position)
    -- the acceleration should increase with lenToAim
    -- till the springConstantAccelerationDistance is reached
    springFactor =
        springAcceleration conf / fromKachel 1
    -- drag to let the swinging stop
    drag = scale dragDirection dragLen
    dragLen = constantDrag +~ dynamicDrag
    constantDrag = frictionFactor conf * mass
    dynamicDrag = dragFactor conf * mass
        * len velocity / platformStandardVelocity
    dragDirection = normalizeIfNotZero (negateAbelian velocity)
