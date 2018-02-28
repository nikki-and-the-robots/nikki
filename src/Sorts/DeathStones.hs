{-# language ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, ViewPatterns, NamedFieldPuns #-}

module Sorts.DeathStones (
    sorts,
    laserAnimationFrameTime,
    mergeDeathStones,
  ) where


import Safe

import Data.Abelian
import Data.Typeable
import Data.Indexable (Indexable, append, optimizeMerge)
import qualified Data.Indexable as I
import Data.List (isPrefixOf, transpose)
import Data.Maybe

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base

import Sorts.Tiles.Baking (StaticPixmap(..), bakeStaticPixmaps)


-- * configuration

type StoneDescription = (SortId, [String], Offset Double, Size Double)

laserDir = "deathstones" </> "lasers"

stones :: [StoneDescription]
stones =
    -- horizontal
    (SortId "deathstones/lasers/horizontal",
     (laserDir </> "laser-horizontal_00") :
     (laserDir </> "laser-horizontal_01") :
     [],
     Position 1 17,
     fmap fromUber $ Size 8 5) :
    (SortId "deathstones/lasers/horizontal-small",
     (laserDir </> "laser-horizontal-small_00") :
     (laserDir </> "laser-horizontal-small_01") :
     [],
     Position 1 17,
     fmap fromUber $ Size 1 5) :

    -- vertical
    (SortId "deathstones/lasers/vertical",
     (laserDir </> "laser-vertical_00") :
     (laserDir </> "laser-vertical_01") :
     [],
     Position 17 1,
     fmap fromUber $ Size 5 8) :
    (SortId "deathstones/lasers/vertical-small",
     (laserDir </> "laser-vertical-small_00") :
     (laserDir </> "laser-vertical-small_01") :
     [],
     Position 17 1,
     fmap fromUber $ Size 5 1) :

    []

laserAnimationFrameTime :: Seconds = 0.1


-- * loading

sorts :: [IO (Maybe Sort_)]
sorts = map ((Just <$>) . loadStone) stones

loadStone :: StoneDescription -> IO Sort_
loadStone (sortId, imageNames, offset, size) = do
    images <- mapM (loadPixmap offset size) =<< mapM mkPngFile imageNames
    return $ Sort_ $ SSort sortId images (mkAnimation images [laserAnimationFrameTime])
  where
    mkPngFile :: String -> IO FilePath
    mkPngFile imageName = getDataFileName (pngDir </> imageName <.> "png")

data SSort = SSort {
    sortId :: SortId,
    pixmaps :: [Pixmap],
    animation :: Animation Pixmap
  }
    deriving (Show, Typeable)

unwrapLaser :: Sort_ -> Maybe SSort
unwrapLaser (Sort_ sort) | "deathstones/lasers/" `isPrefixOf` getSortId (Base.sortId sort) =
    cast sort
unwrapLaser _ = Nothing

data Stone = Stone {
    chipmunk :: Chipmunk
  }
    deriving (Show, Typeable)


instance Sort SSort Stone where
    sortId (SSort s _ _) = s

    size (SSort _ pixmaps _) = pixmapSize $ head pixmaps

    renderIconified sort ptr =
        renderPixmapSimple ptr $ head $ pixmaps sort

    initialize _app _ Nothing sort editorPosition Nothing _ = io $ do
        let (_shapes, baryCenterOffset) = mkShapes $ size sort
            position = epToPosition (size sort) editorPosition
        return $ Stone $ ImmutableChipmunk position 0 baryCenterOffset []
    initialize _app _ (Just space) sort editorPosition Nothing _ = io $ do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            shapesWithAttributes = map (mkShapeDescription shapeAttributes) shapes
            position = position2vector (epToPosition (size sort) editorPosition)
                            +~ baryCenterOffset
            bodyAttributes = StaticBodyAttributes position
        chip <- CM.initChipmunk space bodyAttributes shapesWithAttributes baryCenterOffset
        return $ Stone chip

    immutableCopy (Stone x) = CM.immutableCopy x >>= return . Stone

    chipmunks (Stone x) = singleton x

    isUpdating = const False

    renderObject _ _ o sort _ptr _offset now = do
        (pos, _) <- getRenderPositionAndAngle (chipmunk o)
        let pixmap = pickAnimationFrame (animation sort) now
        return $ singleton $ RenderPixmap pixmap pos Nothing


-- * merging

mergeDeathStones :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
mergeDeathStones ixs =
    append otherObjects (ftoList $ inner deathStones)
  where
    otherObjects = I.filter (isNothing . unwrapLaser . editorSort) ixs
    deathStones = I.filter (isJust . unwrapLaser . editorSort) ixs
    inner = fmap wrapMerging >>> optimizeMerge maybeMerge >>> fmap unwrapMerging


data Merging
    = OtherObject (EditorObject Sort_)
    | SimpleLaser (EditorObject SSort)
    | Merged [EditorObject SSort] EditorPosition (Size Double)

wrapMerging :: EditorObject Sort_ -> Merging
wrapMerging o@(EditorObject sort p _) =
    case unwrapLaser sort of
        Just laserSort -> SimpleLaser $ EditorObject laserSort p Nothing
        Nothing -> OtherObject o

unwrapMerging :: Merging -> EditorObject Sort_
unwrapMerging (OtherObject x) = x
unwrapMerging (SimpleLaser (EditorObject sort p Nothing)) = EditorObject (Sort_ sort) p Nothing
unwrapMerging (Merged objects p s) = EditorObject (Sort_ $ MergedLasersSort objects p s) p Nothing

-- | smart constructor for Merged
merged :: [EditorObject SSort] -> Merging
merged elements =
    Merged elements position size_
  where
    position = EditorPosition minX maxY
    size_ = Size (maxX - minX) (maxY - minY)
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys
    xs = map editorX allCorners
    ys = map editorY allCorners

    -- all lower left and upper right corners
    allCorners = concat $ map
        (\ eo ->
            (eo ^. editorPosition) :
            upperRight eo :
            [])
        elements

    upperRight :: EditorObject SSort -> EditorPosition
    upperRight eo =
        let (Size width height) = size $ editorSort eo
        in (eo ^. editorPosition +~ EditorPosition width (- height))


maybeMerge :: Merging -> Merging -> Maybe Merging
maybeMerge OtherObject{} _ = Nothing
maybeMerge _ OtherObject{} = Nothing
maybeMerge a b
    | sameHeight a b && sameY a b && horizontallyAdjacent a b
    = Just $ justMerge a b
maybeMerge a b
    | sameWidth a b && sameX a b && verticallyAdjacent a b
    = Just $ justMerge a b
maybeMerge _ _ = Nothing

-- | Just merges the two objects.
-- PRE: no OtherObjects.
justMerge :: Merging -> Merging -> Merging
justMerge a b =
    merged (elements a ++ elements b)
  where
    elements (SimpleLaser x) = [x]
    elements (Merged xs _ _) = xs


-- ** predicates for merging

sameHeight, sameWidth, sameX, sameY :: Merging -> Merging -> Bool
sameHeight = (==) `on` mergingHeight
sameWidth  = (==) `on` mergingWidth
sameX      = (==) `on` mergingX
sameY      = (==) `on` mergingY

horizontallyAdjacent, verticallyAdjacent :: Merging -> Merging -> Bool
horizontallyAdjacent a b =
    (mergingX a + mergingWidth a == mergingX b) ||
    (mergingX b + mergingWidth b == mergingX a)
verticallyAdjacent a b =
    (mergingY a - mergingHeight a == mergingY b) ||
    (mergingY b - mergingHeight b == mergingY a)


-- ** merging getter

mergingHeight, mergingWidth :: Merging -> Double
mergingHeight (SimpleLaser eo)  = height $ size $ editorSort eo
mergingHeight (Merged _ _ size) = height size
mergingWidth  (SimpleLaser eo)  = width  $ size $ editorSort eo
mergingWidth  (Merged _ _ size) = width  size

mergingX, mergingY :: Merging -> Double
mergingX (SimpleLaser eo) = editorX (eo ^. editorPosition)
mergingX (Merged _ p _)   = editorX p
mergingY (SimpleLaser eo) = editorY (eo ^. editorPosition)
mergingY (Merged _ p _)   = editorY p


-- * merged lasers

data MergedLasersSort =
    MergedLasersSort {
        mergedLasers :: [EditorObject SSort],
        _a :: EditorPosition,
        _b :: (Size Double)
      }
  deriving (Show, Typeable)

data MergedLasers
    = MergedLasers {
        mlChipmunk :: Chipmunk,
        mlPixmap :: Animation Pixmap
      }
  deriving (Show, Typeable)

-- | Bakes the pixmaps of the merged lasers to one pixmap
bakeMergedLasers :: EditorPosition -> [EditorObject SSort] -> IO (Animation Pixmap)
bakeMergedLasers anchor merged = do
    let transposedPixmaps :: [[Pixmap]]
        transposedPixmaps = transpose $ map (pixmaps . editorSort) merged
        pixmapsWithPositions :: [[(EditorObject SSort, Pixmap)]]
        pixmapsWithPositions = map (\ ps -> zip merged ps) transposedPixmaps
    bakedPixmaps <- fmapM (bakeStaticPixmaps . map convert) pixmapsWithPositions
    let offsetPixmaps = zipWith (\ bp op -> (pixmapOffset ^: (+~ (op ^. pixmapOffset))) bp)
                            bakedPixmaps (map (headNote "bakedMergedLasers") transposedPixmaps)
                                         -- this offset is only true for the used lasers,
                                         -- where every pixmap has the same offset
    return $ mkAnimation offsetPixmaps [laserAnimationFrameTime]
  where
    convert :: (EditorObject SSort, Pixmap) -> StaticPixmap ()
    convert (eo, Pixmap{pixmap, pixmapImageSize}) =
        StaticPixmap ep pixmapImageSize pixmap zero ()
      where
        ep = epToPosition size_ (eo ^. editorPosition -~ anchor)
        size_ = size $ editorSort eo


instance Sort MergedLasersSort MergedLasers where
    sortId _ = SortId "deathstones/lasers/merged"

    size (MergedLasersSort _ _ size) = size

    renderIconified _sort _ptr =
        error "renderIconified: use unmerged lasers"

    initialize _app _ Nothing sort editorPosition Nothing _ = io $ do
        let (_shapes, baryCenterOffset) = mkShapes $ size sort
            position = epToPosition (size sort) editorPosition
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        pix <- bakeMergedLasers editorPosition (mergedLasers sort)
        return $ MergedLasers chip pix
    initialize _app _ (Just space) sort editorPosition Nothing _ = io $ do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            shapesWithAttributes = map (mkShapeDescription shapeAttributes) shapes
            position = position2vector (epToPosition (size sort) editorPosition)
                            +~ baryCenterOffset
            bodyAttributes = StaticBodyAttributes position
        chip <- CM.initChipmunk space bodyAttributes shapesWithAttributes baryCenterOffset
        pix <- bakeMergedLasers editorPosition (mergedLasers sort)
        return $ MergedLasers chip pix

    immutableCopy (MergedLasers c p) =
        MergedLasers <$> CM.immutableCopy c <*> pure p

    chipmunks = return . mlChipmunk

    isUpdating = const False

    renderObject _ _ o _sort _ptr _offset now = do
        (pos, _) <- getRenderPositionAndAngle (mlChipmunk o)
        let pixmap = pickAnimationFrame (mlPixmap o) now
        return $ singleton $ RenderPixmap pixmap pos Nothing


-- * physics initialisation

shapeAttributes = ShapeAttributes {
    elasticity = 0,
    friction = 0,
    collisionType = DeadlyPermeableCT
  }


mkShapes :: Size Double -> ([ShapeType], Vector)
mkShapes size = ([box], baryCenterOffset)
  where
    box = mkRect (negateAbelian half) size
    half = size2position $ fmap (/ 2) size
    baryCenterOffset = position2vector half
