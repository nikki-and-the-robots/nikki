{-# language ViewPatterns, ScopedTypeVariables, DeriveDataTypeable #-}

module Sorts.Robots.PathRobots.Path where


import Safe

import Data.Abelian
import Data.Typeable
import Data.Generics
import Data.Accessor

import Physics.Chipmunk hiding (start, end)

import Graphics.Qt hiding (scale)

import Base

import Utils hiding (distance)

import Sorts.Robots.PathRobots.Configuration

import Editor.Scene.Rendering
import Editor.Scene.Rendering.Helpers


-- | Describes the path of a platform.
-- A platform path can be thought of as a cycle of nodes
-- (that make up a cycle of segments).
data Path
    = Path {
        segments :: [Segment],
        distanceToGuidePoint :: CpFloat,
        pathLength :: CpFloat
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
    sumLength :: [Segment] -> CpFloat
    sumLength = sum . map segmentLength

    -- wraps the path in a SingleNode when platform is initially switched off
    wrap = if active then id else SingleNode (head list) . Just

-- | returns the currently active segment
currentSegment :: Path -> Segment
currentSegment Path{segments = (a : _)} = a

data Segment = Segment {
    start :: Vector,
    end :: Vector,
    segmentLength :: CpFloat
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
guidePoint :: [Segment] -> CpFloat -> Vector
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
    tmpNewDistance = distance + updateStepQuantum * platformStandardVelocity
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

-- | Applies a force to the path robot.
-- The force is composed of an antiGravity and a path force
-- that will let the platform follow its path.
applyPathRobotForce :: Chipmunk -> Path -> IO ()
applyPathRobotForce chip path = do
    antiGravity <- getAntiGravity chip
    motion <- getPathForce chip path

    let force = antiGravity +~ motion
    applyOnlyForce (body chip) force zero
    return ()

-- | calculates the force that lets the platform hover
getAntiGravity :: Chipmunk -> IO Vector
getAntiGravity chip = do
    m <- getMass chip
    return (Vector 0 (- gravity * m))

-- | calculates the force that moves the platform to the next path node
getPathForce :: Chipmunk -> Path -> IO Vector
getPathForce chip path = do
    m <- getMass chip
    p <- getPosition chip
    v <- get $ velocity $ body chip
    return $ mkPathForce path m p v

-- | (pure) calculation of the path force.
mkPathForce :: Path -> Mass -> Vector -> Vector -> Vector
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


-- * object edit mode

oemMethods :: Size Double -> OEMMethods
oemMethods size = OEMMethods
    (OEMState . initialState size)
    (fmap OEMState . unpickle size)

data OEMPath = OEMPath {
    oemRobotSize :: Size Double,
    oemStepSize :: Int,
    oemCursor_ :: EditorPosition,
    pathPositions :: OEMPathPositions,
    oemActive :: Bool
  }
    deriving (Show, Typeable, Data)

oemCursor :: Accessor OEMPath EditorPosition
oemCursor = accessor oemCursor_ (\ a r -> r{oemCursor_ = a})

instance IsOEMState OEMPath where
    oemEnterMode _ = id
    oemUpdate _ = updateOEMPath
    oemNormalize _ = id
    oemRender ptr app config = renderOEMState app config ptr
    oemPickle (OEMPath _ _ cursor path active) =
        show ((cursor, getPathList path, active) :: PickleType)
    oemHelp = const oemHelpText

type PickleType = (EditorPosition, [EditorPosition], Bool)
    -- last component saves, if the path robot is activated or not.
    -- This means different things in different robots, though.

unpickle :: Size Double -> String -> Maybe OEMPath
unpickle size (readMay -> Just ((cursor, (start : path), active) :: PickleType)) =
    Just $ OEMPath size (fromKachel 1) cursor (OEMPathPositions start path) active
unpickle _ _ = Nothing

-- | use the position of the object as first node in Path
initialState :: Size Double -> EditorPosition -> OEMPath
initialState size p = OEMPath size (fromKachel 1) p (OEMPathPositions p []) True

data OEMPathPositions =
    OEMPathPositions {
        startPosition :: EditorPosition,
        positions :: [EditorPosition]
      }
  deriving (Show, Typeable, Data)

getPathList :: OEMPathPositions -> [EditorPosition]
getPathList (OEMPathPositions start path) = start : path

-- | Adds a point to the path.
addPathPoint :: EditorPosition -> OEMPathPositions -> OEMPathPositions
addPathPoint point (OEMPathPositions start path) =
    OEMPathPositions start (path +: point)

-- | removes the last added point at the given position, if it exists.
removePathPoint :: EditorPosition -> OEMPathPositions -> OEMPathPositions
removePathPoint point (OEMPathPositions start path) =
    OEMPathPositions start (reverse $ deleteNeedle point $ reverse path)
  where
    -- deletes the first occurence of a given element
    deleteNeedle :: Eq a => a -> [a] -> [a]
    deleteNeedle needle list = case span (/= needle) list of
        (before, _needle : after) -> before ++ after
        (before, []) -> before


-- * oem logic

updateOEMPath :: Button -> OEMPath -> OEMUpdateMonad OEMPath
updateOEMPath (KeyboardButton key _ _) oem@(OEMPath size cursorStep cursor path active) =
    case key of
        LeftArrow -> return $ oemCursor ^: (-~ EditorPosition cursorStepF 0) $ oem
        RightArrow -> return $ oemCursor ^: (+~ EditorPosition cursorStepF 0) $ oem
        UpArrow -> return $ oemCursor ^: (-~ EditorPosition 0 cursorStepF) $ oem
        DownArrow -> return $ oemCursor ^: (+~ EditorPosition 0 cursorStepF) $ oem
        -- append new path node
        k | isEditorA k -> return $ OEMPath size cursorStep cursor (addPathPoint cursor path) active
        -- delete path node
        k | isEditorB k -> return $ OEMPath size cursorStep cursor (removePathPoint cursor path) active
        W -> return $ oem{oemStepSize = cursorStep * 2}
        S -> return $ oem{oemStepSize = max 1 (cursorStep `div` 2)}
        Space -> return $ oem{oemActive = not active}
        _ -> oemNothing
  where
    cursorStepF :: Double = fromIntegral cursorStep
updateOEMPath _ _ = oemNothing


renderOEMState :: Sort sort a => Application -> Configuration -> Ptr QPainter
    -> EditorScene sort -> OEMPath -> IO ()
renderOEMState app config ptr scene
  (OEMPath robotSize stepSize cursor pathPositions oemActive) = do
    offset <- transformation ptr cursor robotSize
    renderScene offset
    renderCursor offset
    let stepSizeF = fromIntegral stepSize
    renderCursorStepSize app config ptr $ EditorPosition stepSizeF stepSizeF
  where
    renderScene offset =
        renderObjectScene ptr offset scene
    renderCursor offset =
        drawColoredBox ptr (epToPosition robotSize cursor +~ offset) robotSize 4 yellow

renderOEMPath :: Size Double -> Ptr QPainter -> Offset Double -> [EditorPosition]
    -> IO ()
renderOEMPath size ptr offset paths = do
    setPenColor ptr green 4
    mapM_ (renderLine size ptr) (adjacentCyclic paths)
    mapM_ (drawPathNode size ptr) paths

renderLine :: Size Double -> Ptr QPainter -> (EditorPosition, EditorPosition) -> IO ()
renderLine size ptr (a, b) =
    drawLine ptr (epToCenterPosition size a) (epToCenterPosition size b)

drawPathNode :: Size Double -> Ptr QPainter -> EditorPosition -> IO ()
drawPathNode size ptr n =
    fillRect ptr (epToPosition size n)
        size
        (alpha ^: (* 0.4) $ yellow)


-- * oem help text

oemHelpText :: String =
    "Arrow keys: move cursor\n" ++
    "Ctrl: add new path node\n" ++
    "Shift: remove existing node from path\n" ++
    "Space: change initial state of platform (on / off)\n" ++
    "W, S: change cursor step size"
