{-# language ViewPatterns, ScopedTypeVariables, StandaloneDeriving,
    TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}

-- | module for evaluating attributes of our beloved rectangles

module Physics.Chipmunk.StickyEdges.Tests.Properties where

import           Control.Exception
import           Data.Abelian
import           Data.Typeable
import           Test.QuickCheck

import           Physics.Chipmunk
import           Utils

-- representation of edges

data Edge = Edge {
    from :: Vector,
    to :: Vector
  }
    deriving Show

-- | rotates and edge by (pi / 2)
rotateEdge90 (Edge a b) = Edge (rotateVector90 a) (rotateVector90 b)

rotateVector90 :: Vector -> Vector
rotateVector90 (Vector x y) = Vector (- y) x

rotateShapeType90 :: ShapeType -> ShapeType
rotateShapeType90 (Polygon vs) = Polygon $ map rotateVector90 vs

swapEdge :: Edge -> Edge
swapEdge (Edge a b) = Edge b a

delta :: Edge -> Vector
delta (Edge a b) = b -~ a

edgeAngle :: Edge -> Angle
edgeAngle e = foldAngle $ toAngle $ delta e

hasStickyEdges :: [ShapeType] -> Bool
hasStickyEdges l = any (uncurry (stickyEdges l)) $ completeEdges $ concat $ map toEdges l

toEdges :: ShapeType -> [Edge]
toEdges (Polygon l) = map (uncurry Edge) $ adjacentCyclic l

stickyEdges :: [ShapeType] -> Edge -> Edge -> Bool
stickyEdges all a b =
    (edgeAngle a == edgeAngle b &&
    (inner all a b || inner (map rotateShapeType90 all) (rotateEdge90 a) (rotateEdge90 b)))
  where
    inner all a b = innerHorizontal all a b || innerHorizontal all b a
    innerHorizontal all a b =
        horizontal a &&
        horizontal b &&
        vectorY (from a) == vectorY (from b) &&
        ((vectorX (from a) `inside` (minB b, maxB b) && 
        (not (insidePolys False (from a) all)))
        ||
         ((vectorX (to a) `inside` (minB b, maxB b)) &&
        (not (insidePolys False (to a) all))))
    horizontal a = vectorY (delta a) == 0
    minB b = min (vectorX (from b)) (vectorX (to b))
    maxB b = max (vectorX (from b)) (vectorX (to b))

    -- | tests if a given alpha is in a given range (excluding both bounds)
    inside :: Ord a => a -> (a, a) -> Bool
    inside x (a, b) = x > a && x < b


insidePolys :: Bool -> Vector -> [ShapeType] -> Bool
insidePolys countEdges p polys = any (insidePoly countEdges p) polys

insidePoly :: Bool -> Vector -> ShapeType -> Bool
insidePoly countEdges p (Polygon vertices) =
    if any (== p) vertices then
        -- on a corner
        countEdges
    else if any (\ x -> abs x == pi) angles then
        -- on the edge
        countEdges
    else if abs (sum angles) < 0.0000000000001 then
        False
    else
        True
  where
    angles :: [Angle]
    angles = map foldAngle $ map (\ (a, b) -> pointAngle b - pointAngle a) $ adjacentCyclic vertices
    pointAngle :: Vector -> Angle
    pointAngle = (-~ p) >>> toAngle


-- * the two main testing properties

-- | this is the actual predicate, that 'removeStickyEdges' should ensure.
stickyEdgesRemovable :: TestPolygons -> Bool
stickyEdgesRemovable = not . hasStickyEdges . removeStickyEdges testEpsilon . fromTestPolygons

-- | tests if the result of removeStickyEdges misses some areas that the input had
missesArea :: TestPolygons -> Bool
missesArea (fromTestPolygons -> polys) =
    null offenders
  where
    offenders = filter (not . treatedEqually) points
    treatedEqually :: Vector -> Bool
    treatedEqually point =
        on (==) (insidePolys True point) polys outputPolys
    outputPolys = removeStickyEdges testEpsilon polys
    points :: [Vector]
    points = do
        let c = [-rectLimit, -rectLimit + 0.1 .. rectLimit]
        x <- c
        y <- c
        return $ Vector x y


-- * arbitrary values

-- wrapper type in order to be able to put polygons in class Arbitrary whith special constraints:
-- ShapeTypes are  Polygons with four points forming a rectangle, starting with the upper left point,
-- continuing counterclockwise.
-- (this could be done with Test.QuickCheck.Property.forall, but I'm too lazy to
-- change everything now.)
newtype Wrap a = Wrap {unwrap :: a}
  deriving (Show, Typeable)

type TestPolygons = Wrap [Wrap ShapeType]

instance Exception TestPolygons

instance Exception (Wrap Vector, TestPolygons)

deriving instance Typeable Vector

fromTestPolygons :: TestPolygons -> [ShapeType]
fromTestPolygons = unwrap >>> map unwrap

instance Arbitrary TestPolygons where
    arbitrary = Wrap <$> listOf1 arbitrary
    shrink (Wrap l) = map Wrap $ shrink l

instance Arbitrary (Wrap ShapeType) where
    arbitrary = do
        x <- elements [-rectLimit .. (rectLimit - rectSize)]
        y <- elements [-rectLimit .. (rectLimit - rectSize)]
        width <- elements [1 .. rectSize]
        height <- elements [1 .. rectSize]
        return $ Wrap $ Polygon $ [
            Vector x y,
            Vector x (y + height),
            Vector (x + width) (y + height),
            Vector (x + width) y
          ]

rectSize :: Double = 5
rectLimit :: Double = 10
testEpsilon :: Double = 0.3
