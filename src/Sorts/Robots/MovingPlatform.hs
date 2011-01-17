{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
    DeriveDataTypeable #-}

module Sorts.Robots.MovingPlatform (sorts) where


import Safe

import Data.Typeable
import Data.Abelian

import System.FilePath

import qualified Physics.Hipmunk as Hipmunk
import Physics.Chipmunk as CM hiding (Position, radius)

import Graphics.Qt hiding (scale)

import Utils

import Base hiding (cursorStep)

import Object

import Sorts.Nikki as Nikki (walkingVelocity, nikkiMass)
import Sorts.Robots.Configuration
import Sorts.Tiles (tileShapeAttributes)

import Editor.Scene.Rendering
import Editor.Scene.Rendering.Helpers


-- * configuration

-- | The mass of platforms.
-- (gravity has no effect on platforms
platformMass = nikkiMass * 4
-- | the acceleration that can will applied to a platform
-- to let it follow its path
platformAcceleration = 1300
-- | the maximal velocity a platform can accelerate to
maximumPlatformVelocity = 180
-- | the minimal velocity a platform will decelerate to
-- at path nodes
minimumPlatformVelocity = 10
-- | sets the epsilon range for the velocity correction
velocityEpsilon = 5
-- | how much the platform tries to get back on its path
-- (as opposed to going to the end point of the active segment directly)
pathWeight = 0.66


-- * loading

sorts :: RM [Sort_]
sorts = do
    path <- getDataFileName (pngDir </> "robots" </> "platform" </> "horizontal-standard_idle_00" <.> "png")
    pix <- loadPixmap (Position 1 1) path
    return $ [Sort_ $ PSort pix]

data PSort = PSort {
    pix :: Pixmap
  }
    deriving (Show, Typeable)

data Platform
    = Platform {
        platformSize :: Size Double,
        chipmunk :: Chipmunk,
        path :: Path,
        lastNode :: Vector
      }
  deriving (Show, Typeable)

-- | returns the next path node
nextNode :: Platform -> Vector
nextNode p@(path -> (a : _)) = a

-- | Describes the path of a platform.
-- A platform path can be thought of as a cycle of nodes
-- (that make up a cycle of segments).
type Path = [Vector]


instance Sort PSort Platform where
    sortId _ = SortId "robots/platform/standard"
    size = pix >>> pixmapSize

    objectEditModeMethods s = Just (oem s)

    sortRender sort ptr _ =
        renderPixmapSimple ptr (pix sort)

    initialize sort (Just space) ep (Just oemState) = do
        let Size width height = size sort
            baryCenterOffset = Vector (width / 2) (height / 2)

            shape = mkPoly sort
            shapes = [mkShapeDescription shapeAttributes shape]

            pos = qtPosition2Vector (editorPosition2QtPosition sort ep)
                    +~ baryCenterOffset
        chip <- initChipmunk space (bodyAttributes sort pos) shapes baryCenterOffset

        let path = mkPath sort oemState
            lastNode = head path
        return $ Platform (size sort) chip path lastNode

    chipmunks p = [chipmunk p]

    immutableCopy p@Platform{chipmunk} =
        CM.immutableCopy chipmunk >>= \ x -> return p{chipmunk = x}

    updateNoSceneChange sort mode now contacts cd =
        updateSegment >>>>
        passThrough applyPlatformForce

    render platform sort ptr offset now = do
        (position, rad) <- getRenderPosition $ chipmunk platform
        renderPixmap ptr offset position (Just rad) (pix sort)


-- * attributes

mkPoly :: PSort -> ShapeType
mkPoly sort = mkRect (Position (- width / 2) (- height / 2)) size_
  where
    size_@(Size width height) = size sort

bodyAttributes :: PSort -> Vector -> BodyAttributes
bodyAttributes sort pos =
    BodyAttributes pos platformMass infinity

-- | tile friction to allow better walking
shapeAttributes = robotShapeAttributes{friction = friction tileShapeAttributes}


-- * physics behaviour

-- | The platform has an active segment at any time,
-- between (lastNode platform) and (nextNode platform).
-- This operation switches to the next segment if needed.
-- If a switch takes place, an impulse is applied to
-- smoothen behaviour at path nodes.
updateSegment :: Platform -> IO (Platform)
updateSegment p@(path -> [_]) = return p
updateSegment platform@Platform{chipmunk, path = (next : r)} = do
    p <- getPosition chipmunk
    let last = lastNode platform
        closestPathPoint = closestPointOnLineSegment (last, next) p
    if closestPathPoint == next then do
        let newPlatform = platform{path = r +: next, lastNode = next}
        applyEdgeImpulse platform
                (foldAngle $ toAngle (next -~ last))
                (foldAngle $ toAngle (nextNode newPlatform -~ next))
        return newPlatform
      else
        return platform

-- | calculates the impulse to apply when switching path segments
applyEdgeImpulse :: Platform -> Angle -> Angle -> IO ()
applyEdgeImpulse platform last next = do
    let b = body $ chipmunk platform
    m <- get $ Hipmunk.mass b
    v <- get $ velocity b
    let delta = foldAngle (next - last)
        wantedVelocity = rotateVector delta v
        velocityDeviation = wantedVelocity -~ v
        impulse = scale velocityDeviation m
    applyImpulse b impulse zero

-- | Applies a force to the platform.
-- The force is composed of an antiGravity and a path force
-- that will let the platform follow its path.
applyPlatformForce :: Platform -> IO ()
applyPlatformForce platform = do
    antiGravity <- getAntiGravity platform
    motion <- getPathForce platform

    let force = antiGravity +~ motion
    applyOnlyForce (body $ chipmunk platform) force zero
    return ()

-- | calculates the force that lets the platform hover
getAntiGravity :: Platform -> IO Vector
getAntiGravity p = do
    m <- get $ Hipmunk.mass $ body $ chipmunk p
    return (Vector 0 (- gravity * m))

-- | calculates the force that moves the platform to the next path node
getPathForce :: Platform -> IO Vector
getPathForce platform = do
    m <- get $ Hipmunk.mass $ body $ chipmunk platform
    p <- getPosition $ chipmunk platform
    v <- get $ velocity $ body $ chipmunk platform
    return $ mkPathForce platform m p v

-- | (pure) calculation of the path force.
mkPathForce :: Platform -> Double -> Vector -> Vector -> Vector
mkPathForce platform m p v =
    -- the force will always have the same length (or 0)
    scale force forceLen
  where
    forceLen = m * platformAcceleration

    -- | point on the active segment that is closest to
    -- the platform's position
    closestPathPoint =
        closestPointOnLineSegment (lastNode platform, nextNode platform) p

    -- | point where the platform is headed.
    aim = addWeightedVectors
        (closestPathPoint, pathWeight)
        (nextNode platform, (1 - pathWeight))
    -- | from the platform to the aim
    toAim = aim -~ p
    lenToAim = len toAim
    wantedVelocityLen :: Double
    wantedVelocityLen = mkWantedVelocityLen $ len (nextNode platform -~ p)
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


-- * object edit mode

oem :: PSort -> ObjectEditModeMethods Sort_
oem sort = ObjectEditModeMethods {
    oemInitialState = \ ep -> show $ initialState ep,
    oemEnterMode = \ scene -> id,
    oemUpdate = \ scene key state -> show $ updateOEMPath key (read state),
    oemRender = \ ptr scene state -> renderOEMPath sort ptr scene (read state)
  }

data OEMPath = OEMPath {
    oemCursor :: EditorPosition,
    oemPath :: [EditorPosition]
  }
    deriving (Show, Read)

-- | reads an OEMPath and returns the path for the game
mkPath :: PSort -> String -> Path
mkPath sort s =
    let (OEMPath cursor path) = readNote "expected: Sorts.Robots.MovingPlatform.OEMPath" s
    in map (epToCenterVector sort) path

modifyCursor :: (EditorPosition -> EditorPosition) -> OEMPath -> OEMPath
modifyCursor f p = p{oemCursor = f (oemCursor p)}

-- | use the position of the object as first node in Path
initialState :: EditorPosition -> OEMPath
initialState p = OEMPath p [p]


updateOEMPath :: AppButton -> OEMPath -> OEMPath
updateOEMPath button = case button of
    LeftButton -> modifyCursor (-~ EditorPosition cursorStep 0)
    RightButton -> modifyCursor (+~ EditorPosition cursorStep 0)
    UpButton -> modifyCursor (-~ EditorPosition 0 cursorStep)
    DownButton -> modifyCursor (+~ EditorPosition 0 cursorStep)
    -- append new path node
    AButton -> (\ (OEMPath cursor path) -> OEMPath cursor (path +: cursor))
    -- delete path node
    BButton -> (\ (OEMPath cursor path) -> OEMPath cursor (filter (/= cursor) path))
    _ -> id

cursorStep = fromKachel 1


renderOEMPath sort ptr scene (OEMPath cursor paths) = do
    offset <- transformation ptr cursor (size sort)
    renderScene offset
    renderPath offset
    renderCursor offset
  where
    renderScene offset = do
        renderObjectScene ptr offset scene

    renderPath offset = do
        resetMatrix ptr
        translate ptr offset
        setPenColor ptr green 4
        mapM_ renderLine (adjacentCyclic paths)
        setPenColor ptr red 4
        mapM_ drawPathNode paths
    renderLine :: (EditorPosition, EditorPosition) -> IO ()
    renderLine (a, b) =
        drawLine ptr (epToCenterPosition sort a) (epToCenterPosition sort b)
    drawPathNode n =
        drawCircle ptr (epToCenterPosition sort n) 5

    renderCursor offset =
        drawColoredBox ptr (epToPosition sort cursor +~ offset) (size sort) 4 yellow


-- * position conversions

-- from lower left to upper left
epToPosition :: PSort -> EditorPosition -> Position Double
epToPosition = editorPosition2QtPosition

epToCenterPosition :: PSort -> EditorPosition -> Position Double
epToCenterPosition sort ep = epToPosition sort ep +~ fmap (/ 2) (sizeToPosition $ size sort)

epToCenterVector :: PSort -> EditorPosition -> Vector
epToCenterVector sort = qtPosition2Vector . epToCenterPosition sort
