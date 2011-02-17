{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
    DeriveDataTypeable, ScopedTypeVariables #-}

module Sorts.Robots.MovingPlatform (sorts) where


import Safe

import Data.Typeable
import Data.Abelian
import Data.Generics

import System.FilePath

import Physics.Chipmunk as CM hiding (Position, radius)

import Graphics.Qt hiding (scale)

import Utils

import Base hiding (cursorStep)

import Object

import Sorts.Tiles (tileShapeAttributes)
import Sorts.Robots.Configuration

import Sorts.Robots.MovingPlatform.Configuration
import Sorts.Robots.MovingPlatform.Path

import Editor.Scene.Rendering
import Editor.Scene.Rendering.Helpers


-- * loading

sorts :: RM [Sort_]
sorts = do
    path <- getDataFileName (pngDir </> "robots" </> "platform" </> "horizontal-standard_idle_00" <.> "png")
    pix <- loadPixmap (Position 1 1) path
    return $ [Sort_ $ PSort pix]

data PSort = PSort {
    pix :: Pixmap
  }
    deriving (Show, Typeable, Data)

data Platform
    = Platform {
        platformSize :: Size Double,
        chipmunk :: Chipmunk,
        path :: Path
      }
  deriving (Show, Typeable)


instance Sort PSort Platform where
    sortId _ = SortId "robots/platform/standard"
    size = pix >>> pixmapSize

    objectEditMode s = Just $ oemMethods s

    sortRender sort ptr _ =
        renderPixmapSimple ptr (pix sort)

    initialize sort (Just space) ep (Just (OEMState oemState_)) = do
        let Just oemState = cast oemState_
            Size width height = size sort
            baryCenterOffset = Vector (width / 2) (height / 2)

            shape = mkPoly sort
            shapes = [mkShapeDescription shapeAttributes shape]

            pos = qtPosition2Vector (editorPosition2QtPosition sort ep)
                    +~ baryCenterOffset
        chip <- initChipmunk space (bodyAttributes sort pos) shapes baryCenterOffset

        let path = mkPath sort oemState
        return $ Platform (size sort) chip path

    chipmunks p = [chipmunk p]

    immutableCopy p@Platform{chipmunk} =
        CM.immutableCopy chipmunk >>= \ x -> return p{chipmunk = x}

    updateNoSceneChange sort mode now contacts cd =
        updateLogic >>>>
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

updateLogic :: Platform -> IO Platform
updateLogic platform@Platform{chipmunk, path} = do
    path' <- updatePath chipmunk path
    return $ platform{path = path'}

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
    m <- getMass $ chipmunk p
    return (Vector 0 (- gravity * m))

-- | calculates the force that moves the platform to the next path node
getPathForce :: Platform -> IO Vector
getPathForce platform = do
    m <- getMass $ chipmunk platform
    p <- getPosition $ chipmunk platform
    v <- get $ velocity $ body $ chipmunk platform
    return $ mkPathForce (path platform) m p v


-- * object edit mode

oemMethods :: PSort -> OEMMethods
oemMethods sort = OEMMethods
    (OEMState . initialState sort)
    (OEMState . unpickle sort)

data OEMPath = OEMPath {
    oemPSort :: PSort,
    oemStepSize :: Int,
    oemCursor :: EditorPosition,
    oemPath :: OEMPathPositions
  }
    deriving (Show, Typeable, Data)

instance IsOEMState OEMPath where
    oemEnterMode _ = id
    oemUpdate _ = updateOEMPath
    oemRender = renderOEMPath
    oemPickle (OEMPath _ _ cursor path) =
        show ((cursor, getPathList path) :: PickleType)

type PickleType = (EditorPosition, [EditorPosition])

unpickle :: PSort -> String -> OEMPath
unpickle sort (readMay -> Just (cursor, (start : path))) =
    OEMPath sort (fromKachel 1) cursor (OEMPathPositions start path)

-- | reads an OEMPath and returns the path for the game
mkPath :: PSort -> OEMPath -> Path
mkPath sort (OEMPath _ _ cursor path) =
    let (lastNode : nodes) = map (epToCenterVector sort) (getPathList path)
    in Path (deleteConsecutiveTwins (nodes +: lastNode)) lastNode 0
  where
    -- deletes consecutive points in the path that are identical.
    deleteConsecutiveTwins :: Eq a => [a] -> [a]
    deleteConsecutiveTwins = mergeAdjacentCyclicPairs $
        \ a b -> if a == b then Just a else Nothing

modifyCursor :: (EditorPosition -> EditorPosition) -> OEMPath -> OEMPath
modifyCursor f p = p{oemCursor = f (oemCursor p)}

-- | use the position of the object as first node in Path
initialState :: PSort -> EditorPosition -> OEMPath
initialState sort p = OEMPath sort (fromKachel 1) p (OEMPathPositions p [])

data OEMPathPositions =
    OEMPathPositions {
        startPosition :: EditorPosition,
        pathPositions :: [EditorPosition]
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

updateOEMPath :: AppButton -> OEMPath -> OEMPath
updateOEMPath button oem@(OEMPath sort cursorStep cursor path) =
    case button of
        LeftButton -> modifyCursor (-~ EditorPosition cursorStepF 0) oem
        RightButton -> modifyCursor (+~ EditorPosition cursorStepF 0) oem
        UpButton -> modifyCursor (-~ EditorPosition 0 cursorStepF) oem
        DownButton -> modifyCursor (+~ EditorPosition 0 cursorStepF) oem
        -- append new path node
        AButton -> OEMPath sort cursorStep cursor (addPathPoint cursor path)
        -- delete path node
        BButton -> OEMPath sort cursorStep cursor (removePathPoint cursor path)
        (KeyboardButton W _) -> oem{oemStepSize = cursorStep * 2}
        (KeyboardButton S _) -> oem{oemStepSize = max 1 (cursorStep `div` 2)}
        _ -> oem
  where
    cursorStepF :: Double = fromIntegral cursorStep


renderOEMPath :: Sort sort a => Ptr QPainter -> EditorScene sort -> OEMPath -> IO ()
renderOEMPath ptr scene (OEMPath sort stepSize cursor (getPathList -> paths)) = do
    offset <- transformation ptr cursor (size sort)
    renderScene offset
    renderPath offset
    renderCursor offset
    let stepSizeF = fromIntegral stepSize
    renderCursorStepSize ptr $ EditorPosition stepSizeF stepSizeF
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
