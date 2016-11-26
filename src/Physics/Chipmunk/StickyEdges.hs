{-# language DeriveFunctor #-}

-- | There is a problem with multiple shapes forming bigger geometrical structures in chipmunk:
-- shapes, that are moving while touching these shapes, might get stuck on
-- edges between shapes. In our case, this is espacially problematic, as we cannot model everything by hand,
-- cause we have our level editor. Nikki (and probably everthing else that moves) can get stuch in the edges
-- between two tiles, although the two tiles form a geometrical object without these edges. That's happening,
-- Nikkis shapes overlap with one tile and get stuck on the other.
-- This module tries to provide a way o automatically merge Tiles and change them to shapes, that aren't problematic.
-- There is a quickcheck testing framework for this in the testsuite directory.
-- The current version does not produce the correct result for every case yet.

module Physics.Chipmunk.StickyEdges (removeStickyEdges, Rectangle(..), rotateRectangle90) where


import Prelude hiding (Left, Right)

import Data.Abelian
import Data.Directions

import Control.Arrow

import Physics.Hipmunk (ShapeType(Polygon, vertices), Vector(Vector), CpFloat)

import Physics.Chipmunk.Types (vectorX, vectorY)

import Utils


x = vectorX
y = vectorY

-- * non-redundant representation of rectangles

data Rectangle
  = Rectangle {
    start :: Vector,
    width :: CpFloat,
    height :: CpFloat
  }
    deriving (Eq, Show)

-- | returns the lower right corner of rectangles
end :: Rectangle -> Vector
end r = start r +~ Vector (width r) (height r)

modifyWidth :: (CpFloat -> CpFloat) -> Rectangle -> Rectangle
modifyWidth f (Rectangle start w h) =
    Rectangle start (f w) h

-- | rotates the given rectangle by 90 degrees
rotateRectangle90 :: Rectangle -> Rectangle
rotateRectangle90 (Rectangle (Vector x y) w h) =
    Rectangle (Vector (- y - h) x) h w

-- conversions

toRectangle :: ShapeType -> Rectangle
toRectangle (Polygon [Vector x1 y1, Vector x2 y2, Vector x3 y3, Vector x4 y4])
    | x1 == x2 && x3 == x4 && y1 == y4 && y2 == y3 =
        Rectangle (Vector x1 y1) (x3 - x1) (y3 - y1)

fromRectangle :: Rectangle -> ShapeType
fromRectangle (Rectangle (Vector x y) w h) = Polygon [
        Vector x y,
        Vector x (y + h),
        Vector (x + w) (y + h),
        Vector (x + w) y
      ]

withRectangles :: ([Rectangle] -> [Rectangle]) -> [ShapeType] -> [ShapeType]
withRectangles f =
    map toRectangle >>>
    f >>>
    map fromRectangle


-- * actual algorithm

removeStickyEdges :: CpFloat -> [ShapeType] -> [ShapeType]
removeStickyEdges epsilon =
    withRectangles (
        fixpoint moveSides >>>
        mergePairs removeContained
      ) >>>
    removeWedges epsilon

-- removes rectangles that are containesd inside others
removeContained :: Rectangle -> Rectangle -> Maybe [Rectangle]
removeContained a b =
    justWhen
        (x (start a) >= x (start b) && y (start a) >= y (start b) && x (end a) <= x (end b) && y (end a) <= y (end b))
        [b]

-- | enlarges or shrinks rectangles.
-- This is actually programmed just for the right side of rectangles,
-- but with rotating is applied to all sides.
moveSides :: [Rectangle] -> [Rectangle]
moveSides =
    foldr1 (>>>) (replicate 4 (map rotateRectangle90 >>> moveRightSides))

moveRightSides = mergePairs moveRightSide

moveRightSide a b |
    -- rectangles overlap, one horizontal side is flush --> expand rect, form an L-shaped thing
    x (end a) >= x (start b) &&
    x (end a) < x (end b) &&
    ((y (start a) > y (start b) && -- downside is flush
      y (end a) == y (end b)) ||
     (y (start a) == y (start b) && -- upside is flush
      y (end a) < y (end b)))
  = Just [modifyWidth (+ (x (end b) - x (end a))) a, b]

moveRightSide a b |
    -- both horizontal sides are flush --> merge rects
    x (end a) >= x (start b) &&
    x (end a) <= x (end b) &&
    y (start a) == y (start b) &&
    y (end a) == y (end b)
  = Just [Rectangle s w h]
    where
        s = Vector (on min (x . start) a b) (y (start a))
        w = (on max (x . end) a b) - x s
        h = height a

moveRightSide _ _ = Nothing


-- | moves two points (by distance of epsilon) in L-shaped combinations of two shapes to avoid sticky edges
removeWedges :: CpFloat -> [ShapeType] -> [ShapeType]
removeWedges epsilon =
    withModified $
        foldr1 (>>>) $ replicate 4 (mergePairs moveUpperLeftCorner >>> map rotatePolygon90)
  where
    moveUpperLeftCorner :: [Modified Vector] -> [Modified Vector] -> Maybe [[Modified Vector]]
    moveUpperLeftCorner [Same a, b, c, d] other@[p, q, _, s] =
        let b_ = unwrap b
            p_ = unwrap p
            q_ = unwrap q
            s_ = unwrap s
        in if x a == x p_ && x a == x q_ && y a > y p_ && y a < y q_ && y b_ <= y q_ then
            -- a needs to be moved to the right
            let a' = a +~ Vector epsilon 0
            in Just [[Modified a', b, c, d], other]
        else if y a == y p_ && y a == y s_ && x a > x p_ && x a < x s_ then
            -- a needs to be moved down
            let a' = a +~ Vector 0 epsilon
            in Just [[Modified a', b, c, d], other]
        else
            Nothing
    moveUpperLeftCorner _ _ = Nothing -- upper left corner is already modified

withModified :: ([[Modified Vector]] -> [[Modified Vector]]) -> [ShapeType] -> [ShapeType]
withModified f =
    map vertices >>>
    map (map Same) >>>
    f >>>
    map (map unwrap) >>>
    map Polygon

data Modified a = Same {unwrap :: a} | Modified {unwrap :: a}
  deriving (Eq, Functor)


-- | rotates rectangles by 90 degrees
rotatePolygon90 :: [Modified Vector] -> [Modified Vector]
rotatePolygon90 v = map (fmap rotateVector90) (tail v +: head v)

rotateVector90 :: Vector -> Vector
rotateVector90 (Vector x y) = Vector (- y) x

rotateDirection :: Direction -> Direction
rotateDirection DLeft = DUp
rotateDirection DUp = DRight
rotateDirection DRight = DDown
rotateDirection DDown = DLeft
