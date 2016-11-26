{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}

module Sorts.Tiles.Baking (
    bakeTiles,
    boundingBox, -- for testing
    StaticPixmap(..),
    bakeStaticPixmaps, -- for laser baking
  ) where


import Safe

import Data.Abelian
import Data.List (partition)
import Data.Map (Map, empty, lookup, insert)

import Control.Arrow
import Control.Monad.State (evalStateT, get, modify)

import Graphics.Qt as Qt

import Utils

import Base


-- | entry function
-- Returns the list of baked pixmaps along a list of newly created
-- pixmaps, that have to be garbage collected at some point.
bakeTiles :: [(Animation Pixmap, Qt.Position Double)]
    -> IO [(Animation Pixmap, Qt.Position Double)]
bakeTiles =
    return . groupTiles >=>
    bake

data Grouped
    = Grouped [StaticPixmap Bool]
    | Single (Animation Pixmap) (Position Double)
  deriving Show

-- | static and animated pixmaps
data BakePixmap
    = AnimationBakePixmap (Animation Pixmap) (Position Double)
    | NotOverlapping (StaticPixmap Bool)

isAnimationRelated (NotOverlapping (StaticPixmap _ _ _ _ overlappingAnimation)) =
    overlappingAnimation
isAnimationRelated AnimationBakePixmap{} = True

-- | transforms StaticPixmap to Groupeds.
mkGrouped :: BakePixmap -> Grouped
mkGrouped (AnimationBakePixmap a p) = Single a p
mkGrouped (NotOverlapping x) = Grouped [x]


-- | @StaticPixmap position size pixmap offset overlappingWithAnimation@
data StaticPixmap a
    = StaticPixmap (Position Double) (Size Double) (ForeignPtr QPixmap)
            -- offset
            (Position Double)
            -- overlaps fully with an animated area (Bool when used, () otherwise)
            a
  deriving (Show, Eq, Ord)

bpSize :: StaticPixmap Bool -> Size Double
bpSize (StaticPixmap _ s _ _ _) = s

bpPixmap (StaticPixmap _ _ p _ _) = p

setOverlappingAnimation :: a -> StaticPixmap b -> StaticPixmap a
setOverlappingAnimation x (StaticPixmap p s pix o _) =
    StaticPixmap p s pix o x

normalStaticPixmap :: (Pixmap, Position Double) -> StaticPixmap Bool
normalStaticPixmap a@(pix, _pos) =
    let (Rect pos size) = pixmapToRect a
    in StaticPixmap pos size (pixmap pix) zero False

groupTiles :: [(Animation Pixmap, Qt.Position Double)] -> [Grouped]
groupTiles all =
    let (animationRelated, rest) = groupAnimated all
        statics = groupStatic rest
    in animationRelated ++ statics

-- | Returns the animated Tiles (as Grouped) and also those Tiles
-- that overlap with animated Tiles (as Grouped, in rendering Order).
-- They will be rendered beneath the baked tiles.
-- Returns all static tiles (that still have to baked).
groupAnimated :: [(Animation Pixmap, Qt.Position Double)]
    -> ([Grouped], [StaticPixmap Bool])
groupAnimated all =
    (animationRelated, rest)
  where
    baked = concatMap (mkBakePixmap animatedRects) all

    (animationRelated, rest) =
        first (map mkGrouped) $
        second (map (\ (NotOverlapping x) -> x)) $
        partition isAnimationRelated baked

    -- all Rects that are animated
    animatedRects :: [Rect]
    animatedRects = map pixmapAnimationToRect $ filter (not . isStatic . fst) all



mkBakePixmap :: [Rect] -> (Animation Pixmap, Qt.Position Double) -> [BakePixmap]
mkBakePixmap animatedRects (animation, position) =
    if not (isStatic animation) then
        [AnimationBakePixmap animation position]
      else
        map NotOverlapping $
        inner animatedRects $
        normalStaticPixmap (animationHead animation, position)
  where
    inner :: [Rect] -> StaticPixmap Bool -> [StaticPixmap Bool]
    inner (a : r) pixmap =
        concatMap (inner r) $
        convertCutOffs pixmap $
        cutOff pixmap a
    inner [] pixmap = singleton pixmap

    convertCutOffs :: StaticPixmap Bool -> Maybe (StaticPixmap Bool, [StaticPixmap Bool]) -> [StaticPixmap Bool]
    convertCutOffs input cutoffs = case cutoffs of
        Nothing -> [input]
        Just (center, neighbors) ->
            setOverlappingAnimation True center :
            neighbors

-- | Cuts the StaticPixmap in four pieces, to exclude the given Rect.
-- Follows this pattern:
-- -------------------
-- |       top       |
-- -------------------
-- |   |         |   |
-- | l |    c    | r |
-- |   |         |   |
-- -------------------
-- |      base       |
-- -------------------
--
-- Returns Nothing in case of no overlap and Just (c, neighbors) otherwise
cutOff :: StaticPixmap Bool -> Rect -> Maybe (StaticPixmap Bool, [StaticPixmap Bool])

cutOff pixmap rect | not (overlap (bakePixmapToRect pixmap) rect) = Nothing
cutOff (StaticPixmap _ _ _ _ True) _rect = Nothing

cutOff (StaticPixmap pos size pix offset False) (Rect aPos aSize) =
    Just (center, filter positiveSize $ map clipToBaked [top, left, right, base])
  where

    centerHeight =
        min (aPos ^. y_ + height aSize) (pos ^. y_ + height size) -
        max (aPos ^. y_) (pos ^. y_)

    top, left, center, right, base :: StaticPixmap Bool

    top = StaticPixmap pos topSize pix offset False
    topSize = Size (width size) (aPos ^. y_ - pos ^. y_)

    left = StaticPixmap leftPos leftSize pix (offset -~ (leftPos -~ pos)) False
    leftPos = Position (pos ^. x_) (pos ^. y_ + height topSize)
    leftSize = Size (aPos ^. x_ - pos ^. x_) centerHeight

    center = StaticPixmap centerPos centerSize pix centerOffset False
    centerPos = componentWise max pos aPos
    centerSize = position2size (centerLowerLeft -~ centerPos)
    centerLowerLeft = componentWise min
        (pos +~ size2position size)
        (aPos +~ size2position aSize)
    centerOffset = offset -~ fmap (max 0) (aPos -~ pos)

    right = StaticPixmap rightPos rightSize pix (offset -~ (rightPos -~ pos)) False
    rightPos = aPos +~ Position (width aSize) 0
    rightSize = Size (pos ^. x_ + width size - (aPos ^. x_ + width aSize)) centerHeight

    base = StaticPixmap basePos baseSize pix (offset -~ (basePos -~ pos)) False
    basePos = Position (pos ^. x_) (aPos ^. y_ + height aSize)
    baseSize = Size (width size) (pos ^. y_ + height size - (aPos ^. y_ + height aSize))

    -- | reduces the size of the created baked pixmap
    -- so that it does not exceed the size of the given baked pixmap.
    clipToBaked (StaticPixmap cPos cSize pix offset animationRelated) =
        let cPos' = componentWise max cPos pos
            cSize' = componentWise min cSize (size -~ position2size (cPos' -~ pos))
            offset' = offset -~ (cPos' -~ cPos)
        in StaticPixmap cPos' cSize' pix offset' animationRelated

    positiveSize :: StaticPixmap Bool -> Bool
    positiveSize (StaticPixmap _ (Size w h) _ _ _) =
        w > 0 && h > 0


-- | type for representing rectangle areas (disregarding padding pixels).
data Rect = Rect {
    rectPos :: Position Double,
    rectSize :: Size Double
  }
    deriving Show

lowerRight :: Rect -> Position Double
lowerRight (Rect pos size) = pos +~ size2position size

-- | Returns if two Rects overlap.
overlap :: Rect -> Rect -> Bool
overlap (Rect (Position x1 y1) (Size w1 h1)) (Rect (Position x2 y2) (Size w2 h2)) =
    (x1 < xe2 && xe1 > x2) &&
    (y1 < ye2 && ye1 > y2)
  where
    xe1 = x1 + w1
    ye1 = y1 + h1
    xe2 = x2 + w2
    ye2 = y2 + h2

-- | Returns the Rect occupied by a given animation and position.
-- Disregards padding pixel but not offsets.
pixmapAnimationToRect :: (Animation Pixmap, Position Double) -> Rect
pixmapAnimationToRect (anim, pos) =
    pixmapToRect (animationHead anim, pos)

pixmapToRect :: (Pixmap, Position Double) -> Rect
pixmapToRect (pix, pos) =
    Rect
        (pos +~ pix ^. pixmapOffset +~ split 1)
        (pixmapImageSize pix -~ split 2)

bakePixmapToRect :: StaticPixmap Bool -> Rect
bakePixmapToRect (StaticPixmap pos size _ _ _) = Rect pos size


-- * grouping of static tiles
-- (The actual baking algorithm)
groupStatic :: [StaticPixmap Bool] -> [Grouped]
groupStatic = groupStaticH Nothing

groupStaticH :: Maybe Grouped -> [StaticPixmap Bool] -> [Grouped]
groupStaticH Nothing (a : r) =
    groupStaticH (Just (Grouped [a])) r
groupStaticH (Just grouped@(Grouped groupedPixmaps)) r =
    let (_, size) = boundingBox $ map
                        ((\ (Rect p s) -> (p, s)) . bakePixmapToRect)
                        groupedPixmaps
    in if tooBig size then
        -- very big, baking won't be continued
        grouped : groupStaticH Nothing r
      else
        -- bake to the right or down (to get closer to quadratic baked pixmaps)
        let dimensions = if width size > height size then
                    (verticalDimension, horizontalDimension)
                  else
                    (horizontalDimension, verticalDimension)
        -- try fst first, then snd
        in case (extend (fst dimensions) grouped r, extend (snd dimensions) grouped r) of
            (Just (grouped, rest), _) ->
                groupStaticH (Just grouped) rest
            (Nothing, Just (grouped, rest)) ->
                groupStaticH (Just grouped) rest
            (Nothing, Nothing) -> grouped : groupStaticH Nothing r
groupStaticH Nothing [] = []

tooBig size = height size > 512 || width size > 512

-- | Extends the given Grouped if possible. Also returns the rest of the pixmaps.
extend :: Dimension -> Grouped -> [StaticPixmap Bool] -> Maybe (Grouped, [StaticPixmap Bool])
extend d (Grouped grouped) r =
    -- extend the area in the given dimension
    let (position, size) = second (fmap fromIntegral) $
                                boundingBox $ map toPosSize grouped
        upperRight = position +~ (y_ ^= 0) (size2position size)
        lowerLimit = position ^. y_ + height size
    in case searchExtenders d upperRight lowerLimit r of
        Nothing -> Nothing
        Just extenders ->
            let newRight = minimum $
                    map ((^. x_) . lowerRight . bakePixmapToRect) extenders
                extendedRect = Rect position
                    ((width_ ^= newRight - position ^. x_) $ size)
                (extendedAreas, otherAreas) =
--                     trace (show (map (lowerRight . bakePixmapToRect) extenders)) $
                    cutMarked extendedRect r
            in Just ((Grouped (grouped ++ extendedAreas)), otherAreas)
  where
    toPosSize :: StaticPixmap Bool -> (Position Double, Size Double)
    toPosSize (StaticPixmap p s _ _ _) = (p, s)

    -- Cuts the marked (overlapping) Pixmaps, but keep the rendering order.
    -- Returns the StaticPixmaps inside animated areas and all remaining
    -- (possibly cut) StaticPixmaps in correct rendering order.
    cutMarked :: Rect -> [StaticPixmap Bool] -> ([StaticPixmap Bool], [StaticPixmap Bool])
    cutMarked = cutMarkedH ([], [])
    cutMarkedH (extendedsAkk, othersAkk) _extendedRect [] =
        (reverse extendedsAkk, reverse othersAkk)
    cutMarkedH (extendedsAkk, othersAkk) extendedRect (other : r) =
        case cutOff other extendedRect of
            -- doesn't overlap
            Nothing -> cutMarkedH (extendedsAkk, other : othersAkk) extendedRect r
            Just (center, neighbors) ->
                cutMarkedH (center : extendedsAkk, neighbors ++ othersAkk)
                    extendedRect r

    -- overwriting the dimensional accessors
    x_ = posThis d
    y_ = posOther d
    width = (^. sizeThis d)
    height = (^. sizeOther d)
    width_ = sizeThis d
    height_ = sizeOther d


-- | Searches the BPs to extend the currently handled BP.
-- Does not cut them into pieces.
-- Returns the list of all StaticPixmaps with the extenders marked by True.
searchExtenders :: Dimension -> Position Double -> Double -> [StaticPixmap Bool]
    -> Maybe [StaticPixmap Bool]
searchExtenders d p l list =
    inner [] p l list
  where
    inner :: [StaticPixmap Bool] -> Position Double -> Double -> [StaticPixmap Bool]
        -> Maybe [StaticPixmap Bool]
    inner akk searched lowerLimit _l | searched ^. y_ >= lowerLimit =
        Just (reverse akk)
    inner akk searched lowerLimit l =
        case headMay $ filter (isExtender searched) l of
            -- cannot be extended
            Nothing -> Nothing
            Just extender ->
                let newSearched =
                        y_ ^= ((lowerRight $ bakePixmapToRect extender) ^. y_) $
                        searched

                in inner (extender : akk) newSearched lowerLimit l

    isExtender searched (bakePixmapToRect -> bp) =
        contains bp searched &&
        (componentWise (>) (lowerRight bp) searched == split True)

    -- overwriting the dimensional accessors
    x_ = posThis d
    y_ = posOther d
    width = (^. sizeThis d)
    height = (^. sizeOther d)
    width_ = sizeThis d
    height_ = sizeOther d


-- | Returns if a point is on a given Rect (including edges).
contains :: Rect -> Position Double -> Bool
contains rect point =
    (componentWise (>=) point (rectPos rect) == split True) &&
    (componentWise (<=) point (lowerRight rect) == split True)


-- * creating of baked pixmaps

-- | memoized version of bakeStaticPixmaps (working over all Groupeds)
bake :: [Grouped]
    -> IO [(Animation Pixmap, Qt.Position Double)]
bake l =
    evalStateT (mapM inner l) initialMap
  where
            -- [StaticPixmap] is normalized, therefore the positions doesn't have to
            -- be memoized, it's always @split (- 1)@.
    initialMap :: Map [StaticPixmap Bool] Pixmap
    initialMap = empty

    inner (Single a p) = return (a, p)
    inner (Grouped (normalizeStaticPixmaps -> (grouped, normalization))) = do
        m <- get
        convert normalization <$>
          case Data.Map.lookup grouped m of
            Nothing -> do
                result <- io $ bakeStaticPixmaps $ map (setOverlappingAnimation ()) grouped
                modify (insert grouped result)
                return result
            Just result -> return result -- already memoized

    convert :: Position Double -> Pixmap -> (Animation Pixmap, Position Double)
    convert normalization p = (mkStaticAnimation p, split (- 1) +~ normalization)

-- | Normalizes the StaticPixmap positions and offsets
-- so that the first StaticPixmap is on (Position 0 0).
-- For memoization.
normalizeStaticPixmaps :: [StaticPixmap Bool] -> ([StaticPixmap Bool], Position Double)
normalizeStaticPixmaps l@(StaticPixmap p _ _ _ _ : _) =
    (map (addPosition (negateAbelian p)) l, p)
  where
    addPosition add (StaticPixmap p s pix offset oa) =
        StaticPixmap (add +~ p) s pix offset oa

bakeStaticPixmaps :: [StaticPixmap ()] -> IO Pixmap
bakeStaticPixmaps pixmaps = postGUIBlocking $ do
    let (upperLeft, size) = boundingBox $
            map (addPadding . extractRect) pixmaps
    bakedPixmap <- newQPixmapEmpty size
    painter <- newQPainter bakedPixmap
    resetMatrix painter
    translate painter (negateAbelian upperLeft)
    forM_ pixmaps $ \ (StaticPixmap p s pix offset _) ->
        withClipRect painter p s $
            drawPixmapInMemory painter (p -~ split 1 +~ offset) pix
    destroyQPainter painter
    let dsize = fmap fromIntegral size
        resultPixmap = Pixmap bakedPixmap zero dsize dsize
    return resultPixmap

  where
    extractRect (StaticPixmap pos size _ _ _) = (pos, size)
    addPadding (pos, size) =
        (pos -~ split 1, size +~ split 2)


boundingBox :: [(Position Double, Size Double)]
    -> (Position Double, Size Int)
boundingBox pixmaps =
    (Position minX minY,
     fmap ceiling $ Size (maxX - minX) (maxY - minY))
  where
    minX = minimum $ map positionX upperLefts
    minY = minimum $ map positionY upperLefts
    upperLefts = map fst pixmaps

    maxX = maximum $ map positionX lowerRights
    maxY = maximum $ map positionY lowerRights
    lowerRights = map lowerRight pixmaps
    lowerRight (position, size) =
        position +~ size2position size
