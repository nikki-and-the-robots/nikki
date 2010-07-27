{-# language NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK ignore-exports #-}


module Sorts.Tiles (
    sorts,
    Tile,
    unwrapTile,
    unwrapTileSort,
    canBeMerged,
    initializeMerged
  ) where


import Paths
import Utils

import Data.Abelian
import Data.Generics

import Control.Arrow
import Control.Monad

import System.FilePath

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Base.Constants
import Base.Pixmap

import Object


-- * Tile configuration

-- all loaded tiles with offset and size
names :: [(String, Qt.Position Int, Size Double)]
names = [
    ("tiles/tile-standard-white", (Position (- 33) (- 33)), Size 64 64),
    ("tiles/tile-standard-black", (Position (- 1) (- 1)), Size 64 64),
    ("tiles/terminal-frame-editor", (Position (- 1) (- 1)), Size 192 192),
    ("tiles/special-jump", (Position (- 33) (- 33)), Size 1600 320),
    ("multilayers/grid-white", (Position (- 1) (- 1)), Size 512 512),
    ("backgrounds/trailer-01", (Position (- 0) (- 0)), Size 640 480)
  ]


-- * Tile loading


sorts :: IO [Sort_]
sorts = do
--     names <- map dropExtension <$>
--              filter ((== ".png") . takeExtension) <$> 
--              getDirectoryContents editorTileDir
    mapM (\ (a, b, c) -> mkSort a b c) names

mkSort :: String -> Offset Int -> Size Double -> IO Sort_
mkSort name offset size = do
    pngFile <- getDataFileName (pngDir </> name <.> "png")
    pixmap <- newQPixmap pngFile
--     size <- fmap fromIntegral <$> sizeQPixmap pixmap
    return $ Sort_ $ TSort name (Pixmap pixmap size offset)

data TSort
    = TSort {
        name :: String,
        tilePixmap :: Pixmap
      }
    | MergedSort
    deriving (Show, Typeable)


data Tile
    = Tile {
        tchipmunk :: Chipmunk
      }
    | Merged {
        tchipmunk :: Chipmunk,
        tiles :: [(Offset Double, Pixmap)]
      }
  deriving (Show, Typeable)

unwrapTile :: Object_ -> Maybe Tile
unwrapTile (Object_ sort o) = cast o

unwrapTileSort :: Sort_ -> Maybe TSort
unwrapTileSort (Sort_ s) = cast s


instance Sort TSort Tile where
    sortId TSort{name} = SortId name
    sortId MergedSort = SortId "merged"

    size (TSort _ pix) = pixmapSize pix

    sortRender sort@TSort{tilePixmap} =
        sortRenderSinglePixmap tilePixmap sort

    initialize sort@TSort{} Nothing editorPosition Nothing = do
        let -- baryCenterOffset = fmap (/ 2) $ size sort
            pos = editorPosition2QtPosition sort editorPosition
        return $ Tile $ DummyChipmunk{renderPosition = pos}
    initialize sort@TSort{} (Just space) editorPosition Nothing =
        Tile <$>
        initializeBoxes space [(Nothing, size sort)] (sort, editorPosition)

    chipmunk (Tile c) = c
    chipmunk (Merged c _) = c

    render t@Tile{} sort@TSort{tilePixmap} ptr offset _now = do
        (position, rad) <- getRenderPosition $ tchipmunk t
        renderPixmap ptr offset position (Just rad) tilePixmap

    render (Merged chip tiles) _ ptr worldOffset seconds = do
        Qt.resetMatrix ptr
        translate ptr worldOffset
        (position, _) <- getRenderPosition chip

        translate ptr position

        forM_ tiles $ \ (offset, (Pixmap pix _ pixOffset)) -> do
            let pixmapOffset = offset +~ fmap fromIntegral pixOffset
            translate ptr pixmapOffset
            Qt.drawPixmap ptr zero pix
            translate ptr (negateAbelian pixmapOffset)


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
-- data Tile = Sort (Ptr QPixmap) (Size Double)
-- 
-- 

type Box = (Maybe (Offset Double), Size Double)

initializeBoxes :: Space -> [Box] -> (TSort, EditorPosition) -> IO Chipmunk
initializeBoxes space boxes position = do
    let (shapes, baryCenterOffset) = mkShapes boxes
        shapesWithAttributes =
            map (tuple (shapeAttributes TileCT)) shapes
        pos :: Vector
        pos = qtPosition2Vector (uncurry editorPosition2QtPosition position)
                +~ baryCenterOffset
    chip <- initChipmunk space (bodyAttributes pos)
                shapesWithAttributes baryCenterOffset
    return $ chip


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


-- class AnchoredBox ab where
--     boxSize :: ab -> Size Double
--     boxOffset :: ab -> Qt.Position Double
-- 
-- instance AnchoredBox (Size Double) where
--     boxSize = id
--     boxOffset = const zero

{-instance AnchoredBox Sprited where
    boxSize = defaultPixmapSize
    boxOffset _ = zero
-}
-- instance AnchoredBox MergedSprited where
--     boxSize (MergedSprited sprited _ _) = defaultPixmapSize sprited
--     boxOffset (MergedSprited sprited offset animation) = offset

mkShapes :: [Box] -> ([ShapeType], Vector)
mkShapes boxes =
    (lines, baryCenterOffset)
  where
    lines =
        map (shortenLine 1) $
        mergeProlongingLines $
        removeInnerLines $
        concatMap (uncurry mkBoxLines) boxes
    -- not the real barycenter (works for now, because tiles are static)
    baryCenterOffset = Vector wh hh
    wh = w / 2
    hh = h / 2
    (Size w h) = snd $ head boxes

-- mkTileAnimation :: Sprited -> Animation
-- mkTileAnimation sprited =
--     if AnimatedFrameSetType `member` frameSets sprited then
--         initialAnimation (frameSets sprited ! AnimatedFrameSetType) 0
--       else
--         SingleFrameAnimation



-- * line stuff

-- | create lines around a box
mkBoxLines :: Maybe (Offset  Double) -> Size Double -> [ShapeType]
mkBoxLines mOffset (Size w h) =
    map (mapVectors (+ vectorOffset)) lines
  where
    vectorOffset = case mOffset of
        Nothing -> zero
        Just (Qt.Position x y) -> Vector x y
    lines = [
        LineSegment upperLeft upperRight thickness,
        LineSegment upperRight lowerRight thickness,
        LineSegment lowerRight lowerLeft thickness,
        LineSegment lowerLeft upperLeft thickness
--         LineSegment lowerLeft upperMiddle thickness
--         LineSegment upperLeft (upperRight + Vector 0 20) thickness
      ]
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

-- pickPixmap :: TSort -> Tile -> Ptr QPixmap
-- pickPixmap sort t = pixmap sort





-- * Chipmunk optimisation

-- | looks, if two Tiles can be merged (actually just horizontally adjacent)
canBeMerged :: EditorObject -> EditorObject -> Bool
canBeMerged a b =
    horizontallyAdjacent || verticallyAdjacent
  where
    horizontallyAdjacent = sameHeight && sameY && xAdjacent
    sameHeight = withView (height . size . editorSort) (==) a b
    sameY = withView (editorY . editorPosition) (==) a b
    xAdjacent = xDist == searchedXDist
    xDist = withView (editorX . editorPosition) distance a b
    searchedXDist = withView ((/ 2) . width . size . editorSort) (+) a b

    verticallyAdjacent = sameWidth && sameX && yAdjacent
    sameWidth = withView (width . size . editorSort) (==) a b
    sameX = withView (editorX . editorPosition) (==) a b
    yAdjacent = yDist == searchedYDist
    yDist = withView (editorY . editorPosition) distance a b
    searchedYDist = withView ((/ 2) . height . size . editorSort) (+) a b


initializeMerged :: Space -> [EditorObject] -> IO Object_
initializeMerged space objects@(a : _) = do
    chip <- initializeBoxes space (map (first Just) boxes) position
    return $ Object_ MergedSort $
         Merged chip (zip offsets (map tilePixmap sorts))
  where
    toBox :: EditorObject -> (Offset Double, Size Double)
    toBox EditorObject{editorSort, editorPosition} =
        (pos -~ anchor, size editorSort)
      where
        pos :: Qt.Position Double
        pos = editorPosition2QtPosition editorSort editorPosition

    boxes :: [(Offset Double, Size Double)]
    boxes = map toBox objects
    offsets :: [Offset Double]
    offsets = map fst boxes
    sorts :: [TSort]
    sorts = map (unwrap . editorSort) objects
    unwrap x = case unwrapTileSort x of
        Just x -> x

    position :: (TSort, EditorPosition)
    position = (unwrap $ editorSort a, editorPosition a)
    anchor :: Qt.Position Double
    anchor = uncurry editorPosition2QtPosition position


-- mkTempTile :: Vector -> MergedSprited -> UninitializedObject
-- mkTempTile anchor (MergedSprited sprited offset animation) =
--     Tile sprited (anchor + positionToVector offset) animation
-- 
-- -- | makes a MergedSprited to a given offset
-- mkMergedSprited :: Vector -> UninitializedObject -> MergedSprited
-- mkMergedSprited anchor o =
--     MergedSprited (sprited o) offset (animation o)
--   where
--     offset = vectorToPosition (Object.position o - anchor)
-- 
-- 



