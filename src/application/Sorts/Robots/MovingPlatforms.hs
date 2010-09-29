{-# language MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables, FlexibleInstances,
    ViewPatterns, NamedFieldPuns #-}

module Sorts.Robots.MovingPlatforms (sorts) where


import Data.Typeable
import Data.Abelian
import Data.Color

import Control.Monad

import System.FilePath

import Physics.Chipmunk as CM hiding (Position, radius)

import Graphics.Qt hiding (scale)

import Utils
import Paths

import Base.Constants
import Base.Pixmap
import Base.Types (ObjectEditModeMethods(..), EditorPosition(..))
import Base.Events

import Object

import Sorts.Tiles (tileShapeAttributes)
import Sorts.Nikki as Nikki (walkingVelocity, nikkiMass)

import Editor.Scene.Rendering
import Editor.Scene.Rendering.Helpers


sorts :: IO [Sort_]
sorts = do
    path <- getDataFileName (pngDir </> "robots" </> "platform" </> "horizontal-standard_idle_00" <.> "png")
    pix <- loadPixmap 1 path
    return $ [Sort_ $ PSort pix]

data PSort = PSort {
    pix :: Pixmap
  }
    deriving (Show, Typeable)

data Platform
    = Platform {
        platformSize :: Size Double,
        chipmunk :: Chipmunk,
        mode :: Mode,
        lastNode :: Vector,
        debugCmds :: Ptr QPainter -> IO ()
      }
  deriving (Show, Typeable)


data Mode
    = Still Vector
    | Path {nodes :: [Vector]}
  deriving (Show)

instance Sort PSort Platform where
    sortId _ = SortId "robots/platform/standard"
    size = pix >>> pixmapSize

    objectEditModeMethods s = Just (oem s)

    sortRender sort ptr _ =
        renderPixmapSimple ptr (pix sort)

    initialize sort (Just space) ep Nothing = do
        let Size width height = size sort
            baryCenterOffset = Vector (width / 2) (height / 2)

            shape = mkRect (Position (- width / 2) (- height / 2)) (size sort)
            shapes = [mkShapeDescription shapeAttributes shape]

            pos = qtPosition2Vector (editorPosition2QtPosition sort ep)
                    +~ baryCenterOffset
        chip <- initChipmunk space (bodyAttributes (size sort) pos) shapes baryCenterOffset
        modifyApplyForce chip (antiGravity (size sort))

        let (a : r) = (cycle [pos, pos +~ down, pos +~ right +~ down, pos +~ right])
--         let path = Still pos
        return $ Platform (size sort) chip (Path r) a (const $ return ())

    chipmunks p = [chipmunk p]

    immutableCopy p@Platform{chipmunk} =
        CM.immutableCopy chipmunk >>= \ x -> return p{chipmunk = x}


--     updateNoSceneChange :: object -> Seconds -> Contacts -> (Bool, ControlData) -> IO object
    updateNoSceneChange sort now contacts cd =
         updateNextPosition >>>>
--          printNext >>>>
         updatePlatform

    render platform sort ptr offset now = do
        (position, rad) <- getRenderPosition $ chipmunk platform
        renderPixmap ptr offset position (Just rad) Nothing (pix sort)

        debugCmds platform ptr

down = Vector 0 200
right = Vector 400 0

shapeAttributes = tileShapeAttributes

bodyAttributes :: Size Double -> Vector -> BodyAttributes
bodyAttributes size pos = BodyAttributes {
    CM.position         = pos,
    mass                = platformMass size,
    inertia             = infinity
  }



printNext :: Platform -> IO Platform
printNext p = do
    putStrLn (pp (take 3 (nodes (mode p))))
    return p

-- | updates the currently next position
updateNextPosition :: Platform -> IO Platform
updateNextPosition p@(mode -> Still _) = return p
updateNextPosition p@Platform{chipmunk, mode = Path (a : r)} = do
    cp <- getPosition chipmunk
    if len (cp -~ a) < eps then
        return p{mode = Path r, lastNode = a}
      else
        return p
  where
    eps = 10

-- | returns the position that is actually intended to be the platform's position
getNextPosition :: Platform -> Vector
getNextPosition (mode -> Still v) = v
getNextPosition p@(mode -> Path (a : _)) = a

updatePlatform platform = do
    let nextPosition = getNextPosition platform
    position <- getPosition $ chipmunk platform
    velocity <- getVelocity $ body $ chipmunk platform
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










deb :: Vector -> Ptr QPainter -> IO ()
deb v ptr = do
    let start = Position 200 400
        drawVector (Vector x y) r g b = do
            setPenColor ptr r g b 255 3
            drawLine ptr start (start +~ fmap (* 0.2) (Position x y))

    resetMatrix ptr
    setPenColor ptr 255 255 255 255 3
    drawCircle ptr start 20

--     drawVector drag 0 179 255
    drawVector (scale v 0.1) 255 39 39
--     drawVector vel 255 255 0



-- * object edit mode

oem :: PSort -> ObjectEditModeMethods Sort_
oem sort = ObjectEditModeMethods {
    oemInitialState = \ ep -> show $ initialState ep,
    oemEnterMode = \ scene -> id,
    oemUpdate = \ scene key state -> show $ updatePaths key (read state),
    oemRender = \ ptr scene state -> renderPaths sort ptr scene (read state)
  }

data Paths = Paths {
    cursor :: EditorPosition,
    paths :: [EditorPosition]
  }
    deriving (Show, Read)

modifyCursor :: (EditorPosition -> EditorPosition) -> Paths -> Paths
modifyCursor f p = p{cursor = f (cursor p)}

-- | use the position of the object as first node in Path
initialState :: EditorPosition -> Paths
initialState p = Paths p [p]


updatePaths :: AppButton -> Paths -> Paths
updatePaths RightButton p = modifyCursor (+~ EditorPosition cursorStep 0) p
updatePaths _ p = p

cursorStep = fromKachel 1


renderPaths sort ptr scene (Paths cursor paths) = do
    -- render the scene
    offset <- transformation ptr cursor (size sort)
    renderObjectScene ptr offset scene
    -- render cursor box
    let rp = editorPosition2QtPosition sort cursor
    drawColoredBox ptr (rp +~ offset) (size sort) 4 (RGBA 128 128 128 128)


-- show (Paths (editorPosition2QtPosition sort p











