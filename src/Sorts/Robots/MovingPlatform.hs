{-# language MultiParamTypeClasses, ScopedTypeVariables, FlexibleInstances,
    ViewPatterns, NamedFieldPuns, DeriveDataTypeable #-}

module Sorts.Robots.MovingPlatform (sorts) where


import Safe

import Data.Typeable
import Data.Abelian

import Text.Logging

import Control.Monad

import System.FilePath

import Physics.Chipmunk as CM hiding (Position, radius)

import Graphics.Qt hiding (scale)

import Utils

import Base.Paths
import Base.Types (EditorPosition(..), ObjectEditModeMethods(..), Offset, RM, Pixmap(..))
import Base.Types.Events
import Base.Constants
import Base.Pixmap
import Base.Monad

import Object

import Sorts.Nikki as Nikki (walkingVelocity, nikkiMass)
import Sorts.Robots.Configuration

import Editor.Scene.Rendering
import Editor.Scene.Rendering.Helpers


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
        lastNode :: Vector,
        debugCmds :: Ptr QPainter -> Offset Double -> IO ()
      }
  deriving (Show, Typeable)

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

            shape = mkRect (Position (- width / 2) (- height / 2)) (size sort)
            shapes = [mkShapeDescription robotShapeAttributes shape]

            pos = qtPosition2Vector (editorPosition2QtPosition sort ep)
                    +~ baryCenterOffset
        chip <- initChipmunk space (bodyAttributes (size sort) pos) shapes baryCenterOffset
        modifyApplyForce chip (antiGravity (size sort))

        let path = mkPath sort oemState
            lastNode = head path
        return $ Platform (size sort) chip path lastNode (const $ const $ return ())

    chipmunks p = [chipmunk p]

    immutableCopy p@Platform{chipmunk} =
        CM.immutableCopy chipmunk >>= \ x -> return p{chipmunk = x}

    updateNoSceneChange sort mode now contacts cd =
         updateNextPosition >>>>
         updatePlatform

    render platform sort ptr offset now = do
        (position, rad) <- getRenderPosition $ chipmunk platform
        renderPixmap ptr offset position (Just rad) (pix sort)

        debugCmds platform ptr offset

down = Vector 0 200
right = Vector 400 0

bodyAttributes :: Size Double -> Vector -> BodyAttributes
bodyAttributes size pos = BodyAttributes {
    CM.position         = pos,
    mass                = platformMass size,
    inertia             = infinity
  }

-- | updates the currently next position
updateNextPosition :: Platform -> IO Platform
updateNextPosition p@(path -> [_]) = return p
updateNextPosition p@Platform{chipmunk, path = (a : r)} = do
    cp <- getPosition chipmunk
    if len (cp -~ a) < eps then
        return p{path = r, lastNode = a}
      else
        return p
  where
    eps = 10

-- | returns the position that is actually intended to be the platform's position
getNextPosition :: Platform -> Vector
getNextPosition p@(path -> (a : _)) = a

updatePlatform platform = do
    let nextPosition = getNextPosition platform
    position <- getPosition $ chipmunk platform
    velocity <- get $ velocity $ body $ chipmunk platform
    -- drag of the medium (air).
    let drag = calculateDrag size velocity
    -- Force to move the platform to the next position.
    -- This could be a static position, or the next node in the platform's path.
        (toNext, orthogonalCorrection) = calculateToNext (lastNode platform) nextPosition position
        size = platformSize platform
    modifyApplyOnlyForce (chipmunk platform) (antiGravity size +~ drag +~ toNext)
    -- stop the platform when it shouldn't move
    when (drag == zero && toNext == zero) $
        modifyVelocity (chipmunk platform) (const zero)

    return platform{debugCmds = deb orthogonalCorrection}

-- | returns either (Left drag) or (Right impulse)
-- the impulse is to get the velocity to zero.
calculateDrag :: Size Double -> Vector -> Vector
calculateDrag size velocity =
    if len velocity > 5 then
        scale velocity (- 6 * pi * viscosity * platformRadius size)
      else
        zero


calculateToNext :: Vector -> Vector -> Vector -> (Vector, Vector)
calculateToNext lastNode nextNode p =
    (if len toNext < nextEpsilon then
        zero
      else
        scale (normalize toNext) nextForce, scale (normalize toNext) nextForce)
  where
    toNext = scale direction directionWeight +~ scale orthogonalCorrection (1 - directionWeight)
    direction = nextNode -~ p
    lastToNext = nextNode -~ lastNode
    directionAngle = foldAngle $ toAngle direction
    lastToNextAngle = foldAngle $ toAngle lastToNext
    -- correction force that pushes back to the path between the last node and the next.
    orthogonalCorrection =
        rotateVector
            (angleDiff - deg2rad 90 * signum angleDiff)
            direction
    angleDiff = foldAngle (lastToNextAngle - directionAngle)

    -- | how much the normal direction weighs (the rest will be orthogonalCorrection's weight)
    directionWeight =
        max 0
        (- (40 / pi) * (abs (foldAngle (directionAngle - lastToNextAngle))) + 1)


nextEpsilon :: Double
nextEpsilon = 5




-- | since the nextForce should be equal for all platforms we don't actually factor in the correct mass.
nextForce :: Double
nextForce = 5000 * platformMass (Size (fromKachel 3) (fromKachel 1))

platformMass :: Size Double -> Double
platformMass size = (robotKachelMass * a * b) * 0.1
  where
    (a, b) = (toKachel width, toKachel height)
    Size width height = size

robotKachelMass :: Double
robotKachelMass = nikkiMass * 3

platformRadius :: Size Double -> Double
platformRadius (Size w h) = ((w / 2) + (h / 2)) / 2

antiGravity :: Size Double -> Vector
antiGravity s = (Vector 0 (- gravity * platformMass s))


platformMaxVelocity :: Double
platformMaxVelocity = walkingVelocity * 0.5 <<? "platformMaxVelocity"

-- | viscosity (like the nextForce) should not depend on the real mass of the platform
viscosity :: Double
viscosity =
    nextForce / 
    (6 * pi * platformMaxVelocity * platformRadius (Size (fromKachel 3) (fromKachel 1)))










deb :: Vector -> Ptr QPainter -> Offset Double -> IO ()
deb v ptr offset = do
    let start = Position 200 400
        drawVector (Vector x y) r g b = do
            setPenColor ptr (QtColor r g b 255) 3
            drawLine ptr start (start +~ fmap (* 0.2) (Position x y))

    resetMatrix ptr
    setPenColor ptr white 3
    drawCircle ptr start 20

--     drawVector drag 0 179 255
    drawVector (scale v 0.1) 255 39 39
--     drawVector vel 255 255 0












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
