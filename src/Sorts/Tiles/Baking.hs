{-# language ViewPatterns #-}

module Sorts.Tiles.Baking (
    bakeTiles,
    boundingBox, -- for testing
  ) where


import Data.Abelian
import Data.List
import Data.Maybe

import Control.Arrow

import Graphics.Qt as Qt

import Utils

import Base


-- | entry function
bakeTiles :: Application -> [(Animation Pixmap, Qt.Position Double)]
    -> IO [(Animation Pixmap, Qt.Position Double)]
bakeTiles app =
    return . groupTiles >=>
    mapM (bake app) >=>
    return . concat


data Grouped
    = Grouped [BakePixmap]
    | Single (Animation Pixmap) (Position Double)
  deriving Show

data BakePixmap
    = BakePixmap (Position Double) (Size Double) (Ptr QPixmap) (Position Double) Bool
                                                               -- offset         -- overlaps fully with an animated area
    | AnimationBakePixmap (Animation Pixmap) (Position Double)
  deriving Show

bpSize :: BakePixmap -> Size Double
bpSize (BakePixmap _ s _ _ _) = s

setOverlappingAnimation :: Bool -> BakePixmap -> BakePixmap
setOverlappingAnimation x (BakePixmap p s pix o _) =
    BakePixmap p s pix o x

normalBakePixmap :: (Pixmap, Position Double) -> BakePixmap
normalBakePixmap a@(pix, pos) =
    let (Rect pos size) = pixmapToRect a
    in BakePixmap pos size (pixmap pix) zero False

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
    -> ([Grouped], [BakePixmap])
groupAnimated all =
    (animationRelated, rest)
  where
    baked = concatMap (mkBakePixmap animatedRects) all

    (animationRelated, rest) = first (map mkGrouped) $
        partition isAnimationRelated baked

    -- all Rects that are animated
    animatedRects :: [Rect]
    animatedRects = map pixmapAnimationToRect $ filter (not . isStatic . fst) all

isAnimationRelated (BakePixmap _ _ _ _ animationRelated) = animationRelated
isAnimationRelated AnimationBakePixmap{} = True

-- | transforms BakePixmap to Groupeds.
mkGrouped :: BakePixmap -> Grouped
mkGrouped (AnimationBakePixmap a p) = Single a p
mkGrouped x@BakePixmap{} = Grouped [x]


mkBakePixmap :: [Rect] -> (Animation Pixmap, Qt.Position Double) -> [BakePixmap]
mkBakePixmap animatedRects (animation, position) =
    if not (isStatic animation) then
        [AnimationBakePixmap animation position]
      else
        inner animatedRects $ normalBakePixmap (animationHead animation, position)
  where
    inner :: [Rect] -> BakePixmap -> [BakePixmap]
    inner (a : r) pixmap =
        concatMap (inner r) $
        convertCutOffs pixmap $
        cutOff pixmap a
    inner [] pixmap = singleton pixmap

    convertCutOffs :: BakePixmap -> Maybe (BakePixmap, [BakePixmap]) -> [BakePixmap]
    convertCutOffs input cutoffs = case cutoffs of
        Nothing -> [input]
        Just (center, neighbors) ->
            setOverlappingAnimation True center :
            neighbors

-- | Cuts the BakePixmap in four pieces, to exclude the given Rect.
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
cutOff :: BakePixmap -> Rect -> Maybe (BakePixmap, [BakePixmap])

cutOff pixmap rect | not (overlap (bakePixmapToRect pixmap) rect) = Nothing
cutOff p@(BakePixmap _ _ _ _ True) rect = Nothing

cutOff (BakePixmap pos size pix offset False) (Rect aPos aSize) =
    Just (center, filter positiveSize $ map clipToBaked [top, left, right, base])
  where

    centerHeight =
        min (aPos ^. y_ + height aSize) (pos ^. y_ + height size) -
        max (aPos ^. y_) (pos ^. y_)

    top, left, center, right, base :: BakePixmap

    top = BakePixmap pos topSize pix offset False
    topSize = Size (width size) (aPos ^. y_ - pos ^. y_)

    left = BakePixmap leftPos leftSize pix (offset -~ (leftPos -~ pos)) False
    leftPos = Position (pos ^. x_) (pos ^. y_ + height topSize)
    leftSize = Size (aPos ^. x_ - pos ^. x_) centerHeight

    center = BakePixmap centerPos centerSize pix centerOffset False
    centerPos = componentWise max pos aPos
    centerSize = position2size (centerLowerLeft -~ centerPos)
    centerLowerLeft = componentWise min
        (pos +~ size2position size)
        (aPos +~ size2position aSize)
    centerOffset = offset -~ fmap (max 0) (aPos -~ pos)

    right = BakePixmap rightPos rightSize pix (offset -~ (rightPos -~ pos)) False
    rightPos = aPos +~ Position (width aSize) 0
    rightSize = Size (pos ^. x_ + width size - (aPos ^. x_ + width aSize)) centerHeight

    base = BakePixmap basePos baseSize pix (offset -~ (basePos -~ pos)) False
    basePos = Position (pos ^. x_) (aPos ^. y_ + height aSize)
    baseSize = Size (width size) (pos ^. y_ + height size - (aPos ^. y_ + height aSize))

    -- | reduces the size of the created baked pixmap
    -- so that it does not exceed the size of the given baked pixmap.
    clipToBaked (BakePixmap cPos cSize pix offset animationRelated) =
        let cPos' = componentWise max cPos pos
            cSize' = componentWise min cSize (size -~ position2size (cPos' -~ pos))
            offset' = offset -~ (cPos' -~ cPos)
        in BakePixmap cPos' cSize' pix offset' animationRelated

    positiveSize :: BakePixmap -> Bool
    positiveSize (BakePixmap _ (Size w h) _ _ _) =
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
        (pos +~ pix ^. pixmapOffset +~ Position 1 1)
        (pixmapImageSize pix -~ Size 2 2)

bakePixmapToRect :: BakePixmap -> Rect
bakePixmapToRect (BakePixmap pos size _ _ _) = Rect pos size


-- * grouping of static tiles
-- (The actual baking algorithm)
groupStatic :: [BakePixmap] -> [Grouped]
groupStatic = groupStaticH Nothing

groupStaticH :: Maybe Grouped -> [BakePixmap] -> [Grouped]
groupStaticH Nothing (a : r) = groupStaticH (Just (Grouped [a])) r
groupStaticH (Just (Grouped grouped)) r =
    -- TODO | height (bpSize a) <= width (bpSize a) =
    -- extend the area to the right
    let (position, size) = second (fmap fromIntegral) $
                                boundingBox $ map toPosSize grouped
        upperRight = position +~ Position (width size) 0
        lowerLimit = position ^. y_ + height size
    in case searchExtenders upperRight lowerLimit r of
        Nothing -> Grouped grouped : groupStaticH Nothing r
        Just marked ->
            let extenders = map snd $ filter fst marked
                newRight = minimum $
                    map ((^. x_) . lowerRight . bakePixmapToRect) extenders
                extendedRect = Rect position
                    ((width_ ^= newRight - position ^. x_) $ size)
                (extendedAreas, otherAreas) = cutMarked extendedRect marked
--                 cutOffs = map (cutExtender extendedRect) extenders
--                 extendedAreas = map fst cutOffs
--                 cutOffsRest = concatMap snd cutOffs
            in groupStaticH
                (Just (Grouped (grouped ++ extendedAreas)))
                otherAreas
  where
    toPosSize :: BakePixmap -> (Position Double, Size Double)
    toPosSize (BakePixmap p s _ _ _) = (p, s)

    cutMarked :: Rect -> [(Bool, BakePixmap)] -> ([BakePixmap], [BakePixmap])
    cutMarked = cutMarkedH ([], [])
    cutMarkedH (extendedsAkk, othersAkk) extendedRect [] =
        (reverse extendedsAkk, reverse othersAkk)
    cutMarkedH (extendedsAkk, othersAkk) extendedRect ((False, other) : r) =
        cutMarkedH (extendedsAkk, other : othersAkk) extendedRect r
    cutMarkedH (extendedsAkk, othersAkk) extendedRect ((True, extender) : r) =
        -- extender
        let (extenderCenter, neighbors) = cutExtender extendedRect extender
        in cutMarkedH (extenderCenter : extendedsAkk, neighbors ++ othersAkk)
                extendedRect r

    cutExtender :: Rect -> BakePixmap -> (BakePixmap, [BakePixmap])
    cutExtender extendedRect extender = case cutOff extender extendedRect of
        Just x -> x
        Nothing -> error $ show (extendedRect, extender)
groupStaticH Nothing [] = []

-- | Searches the BPs to extend the currently handled BP.
-- Does not cut them into pieces.
-- Returns the list of all BakePixmaps with the extenders marked by True.
searchExtenders :: Position Double -> Double -> [BakePixmap]
    -> Maybe [(Bool, BakePixmap)]
searchExtenders p s =
    inner p s . map (tuple False)
  where
    inner :: Position Double -> Double -> [(Bool, BakePixmap)]
        -> Maybe [(Bool, BakePixmap)]
    inner searched lowerLimit l | searched ^. y_ >= lowerLimit =
        Just l
    inner searched lowerLimit l =
        case firstAndMarked (isExtender searched) l of
            -- cannot be extended
            Nothing -> Nothing
            Just (extender, marked) ->
                let newSearched = Position
                        (searched ^. x_)
                        ((lowerRight $ bakePixmapToRect extender) ^. y_)
                in inner newSearched lowerLimit marked

    isExtender searched (bakePixmapToRect -> bp) =
        contains bp searched &&
        (componentWise (>) (lowerRight bp) searched == Position True True)

    -- | @firstAndMarked p l@ returns the first element that satisfies p
    -- and the input list with an updated mark in th found element.
    firstAndMarked :: (a -> Bool) -> [(Bool, a)] -> Maybe (a, [(Bool, a)])
    firstAndMarked p =
        inner []
      where
        inner akk (a : r) =
            if p (snd a) then
                Just (snd a, reverse akk ++ [first (const True) a] ++ r)
              else inner (a : akk) r
        inner _ [] = Nothing

-- | Returns if a point is on a given Rect (including edges).
contains :: Rect -> Position Double -> Bool
contains rect point =
    (componentWise (>=) point (rectPos rect) == Position True True) &&
    (componentWise (<=) point (lowerRight rect) == Position True True)


-- * creating of baked pixmaps

bake :: Application -> Grouped -> IO [(Animation Pixmap, Qt.Position Double)]

bake _ (Single a p) = return [(a, p)]

bake app (Grouped pixmaps) =
  postGUIBlocking (window app) $ do
    let (upperLeft, size) = boundingBox $
            map (addPadding . extractRect) pixmaps
    bakedPixmap <- newQPixmapEmpty size
    painter <- newQPainter bakedPixmap
    resetMatrix painter
    translate painter (negateAbelian upperLeft)
    forM_ pixmaps $ \ (BakePixmap position _ pix offset _) -> do
        drawPixmap painter (position -~ Position 1 1 +~ offset) pix
    setPaddingPixelsTransparent painter size
    let dsize = fmap fromIntegral size
        resultPixmap = Pixmap bakedPixmap zero dsize dsize
    return $ singleton (mkAnimation [resultPixmap] [42], upperLeft)

  where
    extractRect (BakePixmap pos size _ _ _) = (pos, size)
    addPadding (pos, size) =
        (pos -~ Position 1 1, size +~ Size 2 2)


setPaddingPixelsTransparent :: Ptr QPainter -> Size Int -> IO ()
setPaddingPixelsTransparent ptr size = do
    resetMatrix ptr
    withClearCompositionMode ptr $
        forM_ paddingPixels $ \ pixel ->
            drawPoint ptr pixel
  where
    xs = [0 .. pred (width size)]
    ys = [0 .. pred (height size)]
    top  = map (\ x -> Position x 0) xs
    base = map (\ x -> Position x (pred (height size))) xs
    left = map (Position 0) ys
    right = map (Position (pred (width size))) ys
    paddingPixels = top ++ base ++ left ++ right


boundingBox :: [(Position Double, Size Double)]
    -> (Position Double, Size Int)
boundingBox pixmaps =
    (Position minX minY,
     fmap ceiling $ Size (maxX - minX) (maxY - minY))
  where
    minX = minimum $ map positionX upperLefts
    minY = minimum $ map positionY upperLefts
    upperLefts = map upperLeft pixmaps
    upperLeft (position, size) = position

    maxX = maximum $ map positionX lowerRights
    maxY = maximum $ map positionY lowerRights
    lowerRights = map lowerRight pixmaps
    lowerRight (position, size) =
        position +~ size2position size
