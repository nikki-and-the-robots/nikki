
module Sorts.Tiles.Baking (
    bakeTiles,
    boundingBox, -- for testing
  ) where


import Data.Abelian
import Data.List

import Control.Arrow

import Graphics.Qt as Qt

import Utils

import Base


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
        cutOff pixmap a
    inner [] pixmap = singleton pixmap

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
    cutOff :: BakePixmap -> Rect -> [BakePixmap]

    cutOff pixmap rect | not (overlap (bakePixmapToRect pixmap) rect) = [pixmap]
    cutOff p@(BakePixmap _ _ _ _ True) rect = [p]

    cutOff (BakePixmap pos size pix offset False) (Rect aPos aSize) =
        filter positiveSize $
        map clipToBaked
        [top, left, center, right, base]
      where
        top, left, center, right, base :: BakePixmap

        top = BakePixmap pos topSize pix offset False
        topSize = Size (width size) (y aPos - y pos)

        left = BakePixmap leftPos leftSize pix (offset -~ (leftPos -~ pos)) False
        leftPos = Position (x pos) (y pos + height topSize)
        leftSize = Size (x aPos - x pos) (height aSize)

        center = BakePixmap centerPos centerSize pix centerOffset True
        centerPos = Position (max (x pos) (x aPos)) (max (y pos) (y aPos))
        centerSize = position2size (centerLowerLeft -~ centerPos)
        centerLowerLeft = Position
            (min (x pos + width  size) (x aPos + width  aSize))
            (min (y pos + height size) (y aPos + height aSize))
        centerOffset = offset -~ fmap (max 0) (aPos -~ pos)

        right = BakePixmap rightPos rightSize pix (offset -~ (rightPos -~ pos)) False
        rightPos = aPos +~ Position (width aSize) 0
        rightSize = Size (x pos + width size - (x aPos + width aSize)) (height aSize)

        base = BakePixmap basePos baseSize pix (offset -~ (basePos -~ pos)) False
        basePos = Position (x pos) (y aPos + height aSize)
        baseSize = Size (width size) (y pos + height size - (y aPos + height aSize))

        -- | reduces the size of the created baked pixmap
        -- so that it does not exceed the size of the given baked pixmap.
        clipToBaked (BakePixmap cPos cSize pix offset animationRelated) =
            let cPos' = componentWise max cPos pos
                cSize' = componentWise min cSize (size -~ position2size (cPos' -~ pos))
                offset' = offset -~ (cPos' -~ cPos)
            in BakePixmap cPos' cSize' pix offset' animationRelated

-- todo
-- center without clipping?
-- use componentWise more often

    y = positionY
    x = positionX
    positiveSize :: BakePixmap -> Bool
    positiveSize (BakePixmap _ (Size w h) _ _ _) =
        w > 0 && h > 0


-- | type for representing rectangle areas (disregarding padding pixels).
data Rect = Rect (Position Double) (Size Double)
  deriving Show

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

groupStatic :: [BakePixmap] -> [Grouped]
groupStatic = map (Grouped . singleton)

toQPixmap :: (Pixmap, Position Double) -> (Ptr QPixmap, Position Double, Size Double)
toQPixmap (pix, position) =
    (pixmap pix,
     position +~ (pix ^. pixmapOffset),
     pixmapImageSize pix)

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
