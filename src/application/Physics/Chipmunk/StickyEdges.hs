{-# language ViewPatterns, ScopedTypeVariables #-}

-- | There is a problem with multiple shapes forming bigger geometrical structures in chipmunk:
-- shapes, that are moving while touching these shapes, might get stuck on
-- edges between shapes. In our case, this is espacially problematic, as we cannot model everything by hand,
-- cause we have our level editor. Nikki (and probably everthing else that moves) can get stuch in the edges
-- between two tiles, although the two tiles form a geometrical object without these edges. That's happening,
-- Nikkis shapes overlap with one tile and get stuck on the other.
-- This module tries to provide a way o automatically merge Tiles and change them to shapes, that aren't problematic.
-- There is a quickcheck testing framework for this in the testsuite directory.
-- The current version does not produce the correct result for every case yet.

module Physics.Chipmunk.StickyEdges (removeStickyEdges, tests) where


import Prelude hiding (Left, Right)

import Data.Abelian
import Data.Maybe

import Control.Applicative ((<$>), (<*>))
import Control.Arrow

import Test.QuickCheck

import Physics.Hipmunk (ShapeType(Polygon, vertices), Vector(Vector), toAngle, Angle)

import Utils hiding (tests)

import Physics.Chipmunk.Types (foldAngle, vectorX, vectorY, rotateVector)


x = vectorX
y = vectorY

-- * non-redundant representation of rectangles

data Rectangle
  = Rectangle {
    start :: Vector,
    width :: Double,
    height :: Double
  }
    deriving (Eq, Show)

instance Arbitrary Rectangle where
    arbitrary = Rectangle <$> arbitrary <*> arbitrary <*> arbitrary

-- | returns the lower right corner of rectangles
end :: Rectangle -> Vector
end r = start r +~ Vector (width r) (height r)

modifyWidth :: (Double -> Double) -> Rectangle -> Rectangle
modifyWidth f (Rectangle start w h) = Rectangle start (f w) h

-- | rotates the given rectangle by 90 degrees
rotateRectangleHalfPi :: Rectangle -> Rectangle
rotateRectangleHalfPi (Rectangle (Vector x y) w h) =
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


-- * actual algorithm

removeStickyEdges :: [ShapeType] -> [ShapeType]
removeStickyEdges =
    map toRectangle >>>
    mergePairs removeContained >>>
    moveSides >>>
    map fromRectangle >>>
    removeWedges >>>
    id

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
    moveRightSides >>>
    map rotateRectangleHalfPi >>>
    moveRightSides >>>
    map rotateRectangleHalfPi >>>
    moveRightSides >>>
    map rotateRectangleHalfPi >>>
    moveRightSides >>>
    map rotateRectangleHalfPi >>>
    id

moveRightSides = mergePairs moveRightSide

moveRightSide a b |
    -- moved side is smaller than the right rectangle --> shrink rect
    x (start a) < x (start b) &&
    x (end a) > x (start b) &&
    x (end a) == x (end b) &&
    y (start a) > y (start b) &&
    y (end a) < y (end b)
  = Just [modifyWidth (subtract (x (end a) - x (start b))) a, b]
moveRightSide a b |
    -- rectangles overlap, one horizontal side is flush --> form an L-shaped thing
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
  = Just [Rectangle (start a) (x (end b) - x (start a)) (height a)]
moveRightSide a b = Nothing

-- | moves two points in L-shaped combinations of two shapes to avoid sticky edges
removeWedges =
    foldr1 (>>>) $ replicate 4 (mergePairs removeWedgesFromLowerLeftCorner >>> map rotateShapeTypeHalfPi)
  where
    removeWedgesFromLowerLeftCorner (Polygon [a, b, c, d]) (Polygon [p, q, r, s])
        | (b == q) -- lower left corner is equal
          && x a == x b -- and corners haven't been moved by removeWedges
          && y c == y b
      = if y b - y a > y q - y p then
            -- first rectangle is higher
            Just [Polygon [a, b, c -~ Vector 0 eps, d], Polygon [p +~ Vector eps 0, q, r, s]]
          else
            -- second rectangle is higher
            Just [Polygon [a +~ Vector eps 0, b, c, d], Polygon [p, q, r -~ Vector 0 eps, s]]
    removeWedgesFromLowerLeftCorner _ _ = Nothing

eps = 1

-- | rotates rectangles by 90 degrees
rotateShapeTypeHalfPi :: ShapeType -> ShapeType
rotateShapeTypeHalfPi (Polygon v) = Polygon $ map rotateVectorHalfPi (tail v +: head v)

rotateVectorHalfPi :: Vector -> Vector
rotateVectorHalfPi (Vector x y) = Vector (- y) x




tests =
    testRotateVectorHalfPi .&.
    testRotateRectangleHalfPi

testRotateVectorHalfPi v = v == superApply 4 rotateVectorHalfPi v

testRotateRectangleHalfPi r =
    whenFail (putStrLn ("rotateRectangleHalfPi: " ++ show (r, r'))) 
        (equals r r')
  where
    r' = superApply 4 rotateRectangleHalfPi r
    equals (Rectangle (Vector a b) c d) (Rectangle (Vector p q) r s) =
        a ~= p && b ~= q && c ~= r && d ~= s
