{-# language NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances,
     DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK ignore-exports #-}


module Sorts.Tiles (
    sorts
  ) where


import Utils

-- import Data.Map hiding (map, filter, size)
import Data.Abelian
import Data.Generics

import Control.Applicative ((<$>))

import System.FilePath
import System.Directory

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Base.Constants

import Object.Types
import Object.Contacts


sorts :: IO [TSort]
sorts = do
    pngs <- filter ((== ".png") . takeExtension) <$> getDirectoryContents editorTileDir
    mapM mkSort $ map (editorTileDir </>) pngs

editorTileDir = pngDir </> "tiles" </> "editor"

mkSort :: FilePath -> IO TSort
mkSort png = do
    pixmap <- newQPixmap png
    size <- fmap fromIntegral <$> sizeQPixmap pixmap
    return $ TSort png pixmap size

data TSort = TSort {
    path :: FilePath,
    pixmap :: Ptr QPixmap,
    tsize :: Size Double
  }
    deriving Typeable

data Tile = Tile {
    tchipmunk :: Chipmunk
  }
    deriving Typeable

instance Sort TSort Tile where
    sortId sort = SortId $ dropExtension $ path sort

    size (TSort _ _ size) = fmap (subtract 2) size

    sortRender sort =
        sortRenderSinglePixmap (pixmap sort) sort

    initialize sort space editorPosition Nothing = do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            collisionType_ = TileCT
            shapesWithAttributes =
                map (tuple (shapeAttributes collisionType_)) shapes
            pos :: Vector
            pos = qtPositionToVector (editorPosition2QtPosition sort editorPosition)
                    +~ baryCenterOffset
        chip <- initStaticChipmunk space (bodyAttributes pos)
                    shapesWithAttributes baryCenterOffset
        return $ Tile chip

    chipmunk (Tile c) = c

    update tile _ _ _ = return tile

    render t sort ptr offset seconds = do
        let pixmap = pickPixmap sort t
        renderChipmunk ptr offset pixmap (tchipmunk t)
--         resetMatrix ptr
--         translate ptr offset
--         pos <- getRenderPosition chip
--         translate ptr (fst pos -~ Position 1 1)
-- --         let pixmap = animationPixmap animation s
--         drawPixmap ptr zero pixmap
--     render ptr globalOffset (MergedTile mergeds chipmunk) = do
--         pos <- getRenderPosition chipmunk
--         mapM_ (inner (fst pos)) mergeds
--       where
--         inner pos (MergedSprited sprited anchorOffset animation) = do
--             resetMatrix ptr
--             translate ptr globalOffset
--             translateVector ptr pos
--             translate ptr anchorOffset
--             let pixmap = animationPixmap animation sprited
--             drawPixmap ptr zero pixmap


--     sortId = SortId path,
-- 
--     readFilesFromDisk = do
--         pixmap <- newQPixmap path
--         size <- sizeQPixmap pixmap
--         return $ SimpleTile pixmap size (),
-- 
--     instantiate = \ (SimpleTile pixmap size _) pos -> SimpleTile pixmap size pos,
--     editorRender = \ ptr offset (SimpleTile pixmap _ pos) -> do
--         resetMatrix ptr
--         translate ptr offset
--         translateEditor ptr pos
--         drawPixmap ptr zero pixmap,
--     editorModify = Nothing
--   }
-- 
-- data Tile_ = Sort (Ptr QPixmap) (Size Double)
-- 
-- 
-- 
-- 
-- initChipmunk :: Space -> UninitializedObject -> IO Object
-- initChipmunk space tile@(MergedTile merged pos) = do
--     let (shapes, baryCenterOffset) = mkShapes merged
--         collisionType = toCollisionType tile
--         shapesWithAttributes = map (tuple (shapeAttributes collisionType)) shapes
--     chip <- initStaticChipmunk space (bodyAttributes pos) shapesWithAttributes baryCenterOffset
--     return $ MergedTile merged chip
-- 
-- 
-- initAnimation :: Object -> Object
-- initAnimation o@Tile{sprited} = setAnimation o $ mkTileAnimation sprited
-- initAnimation o@MergedTile{merged} = o{merged = map inner merged}
--   where
--     inner :: MergedSprited -> MergedSprited
--     inner (MergedSprited sprited offset UninitializedAnimation) =
--         MergedSprited sprited offset (mkTileAnimation sprited)
-- 

bodyAttributes :: Vector -> BodyAttributes
bodyAttributes pos =
    StaticBodyAttributes {
        CM.position = pos
      }

shapeAttributes :: MyCollisionType -> ShapeAttributes
shapeAttributes collisionType =
    ShapeAttributes {
        elasticity = 0.5,
        friction = 2,
        CM.collisionType = collisionType
      }


class AnchoredBox ab where
    boxSize :: ab -> Size Double
    boxOffset :: ab -> Qt.Position Double

instance AnchoredBox (Size Double) where
    boxSize = id
    boxOffset = const zero

{-instance AnchoredBox Sprited where
    boxSize = defaultPixmapSize
    boxOffset _ = zero
-}
-- instance AnchoredBox MergedSprited where
--     boxSize (MergedSprited sprited _ _) = defaultPixmapSize sprited
--     boxOffset (MergedSprited sprited offset animation) = offset

mkShapes :: Size Double -> ([ShapeType], Vector)
mkShapes size@(Size w h) =
    (lines, baryCenterOffset)
  where
    lines =
        map (shortenLine 1) $
        mergeProlongingLines $
        removeInnerLines $
        concatMap mkBoxLines [size]
    -- not the real barycenter (works for now, because tiles are static)
    baryCenterOffset = Vector wh hh
    wh = w / 2
    hh = h / 2

-- mkTileAnimation :: Sprited -> Animation
-- mkTileAnimation sprited =
--     if AnimatedFrameSetType `member` frameSets sprited then
--         initialAnimation (frameSets sprited ! AnimatedFrameSetType) 0
--       else
--         SingleFrameAnimation



-- * line stuff

-- | create lines around a box
mkBoxLines :: AnchoredBox b => b -> [ShapeType]
mkBoxLines box =
    map (mapVectors (+ vectorOffset)) lines
  where
    vectorOffset = Vector (positionX (boxOffset box)) (positionY (boxOffset box))
    lines = [
        LineSegment upperLeft upperRight thickness,
        LineSegment upperRight lowerRight thickness,
        LineSegment lowerRight lowerLeft thickness,
        LineSegment lowerLeft upperLeft thickness
--         LineSegment lowerLeft upperMiddle thickness
--         LineSegment upperLeft (upperRight + Vector 0 20) thickness
      ]
    (Size w h) = boxSize box
    wh = w / 2
    hh = h / 2

    upperLeft = Vector (- wh) (- hh)
    upperRight = Vector wh (- hh)
    lowerLeft = Vector (- wh) hh
    lowerRight = Vector wh hh

    thickness = 0.0


-- | removes lines that have no outer relevance
-- these are where $reciprocal$ holds.
-- of course, this works only for lines constructed with mkBoxLines
removeInnerLines :: [ShapeType] -> [ShapeType]
removeInnerLines = removePairs (\ a b -> reciprocal a b)

reciprocal :: ShapeType -> ShapeType -> Bool
reciprocal (LineSegment a b _) (LineSegment x y _) = a == y && b == x

mergeProlongingLines :: [ShapeType] -> [ShapeType]
mergeProlongingLines list = mergePairs lineMerge list


-- returns a merged line, if the two lines can be merged to one
lineMerge :: ShapeType -> ShapeType -> Maybe [ShapeType]
lineMerge (LineSegment a b t1) (LineSegment p q t2) =
    if t1 == t2 && adjacent && sameDirection then
        Just [(uncurry LineSegment newPoints) t1]
      else
        Nothing
  where
    adjacent = a == p || a == q || b == p || b == q
    sameDirection =
        withView toAngle (rangeEpsilonEquals (0, pi)) (a - b) (p - q)

    newPoints :: (Vector, Vector)
    newPoints = case () of
        _ | a == p -> (b, q)
        _ | a == q -> (b, p)
        _ | b == p -> (a, q)
        _ | b == q -> (a, p)
        _ -> es "newPoints" (a, b, p, q)
lineMerge _ _ = Nothing

-- | shortens a line (on both ends) by a padding distance
shortenLine :: Double -> ShapeType -> ShapeType
shortenLine padding (LineSegment a b thickness) =
    LineSegment a' b' thickness
  where
    a' = a + paddingVector
    b' = b - paddingVector
    paddingVector = vmap (* padding) normedBetween
    normedBetween = normalize between
    between = b - a
shortenLine padding x = es "shortenLine" x


-- -- * updating
-- 
-- update :: Seconds -> Object -> IO Object
-- update now (Tile sprited chip animation)
--     | AnimatedFrameSetType `member` frameSets sprited =
--         return $ Tile sprited chip (updateAnimation now AnimatedFrameSetType animation)
-- update _ tile@Tile{} = return tile
-- 
-- initialAnimation :: FrameSet -> Seconds -> Animation
-- initialAnimation frameSet =
--     mkAnimation AnimatedFrameSetType (const inner)
--   where
--     inner :: AnimationPhases
--     inner = AnimationPhases $ zip 
--         (cycle [0..length (frames frameSet) - 1])
--         (repeat 0.3)

pickPixmap :: TSort -> Tile -> Ptr QPixmap
pickPixmap sort t = pixmap sort


