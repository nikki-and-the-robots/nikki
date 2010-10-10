{-# language ViewPatterns #-}

module Physics.Chipmunk.OptimizePolygons (optimizePolygons) where


import Data.Abelian
import Data.Maybe

import Control.Arrow

import Physics.Hipmunk hiding (isConvex)

import Utils

import Physics.Chipmunk.Types


-- | does optimization of polygons (merging, etc.)
optimizePolygons :: [ShapeType] -> [ShapeType]
optimizePolygons =
    map toEdges >>>
    mergePolys isHorizontal >>> -- merge first via edges that are horizontal
    mergePolys (const True) >>>
    addWedges >>>
    map removeUnneededCorners >>>
    map fromEdges

-- * edge representation

data Edge
    = Edge {
        from :: Vector,
        to :: Vector
      }
  deriving (Show, Eq)

toEdges :: ShapeType -> [Edge]
toEdges (Polygon points) = map (uncurry Edge) $ adjacentCyclic points

fromEdges :: [Edge] -> ShapeType
fromEdges edges =
    if any (\ (a, b) -> to a /= from b) (adjacentCyclic edges) then
        error "not a closed polygon"
      else if not $ isConvex edges then
        error "polygon isn't convex"
      else
        Polygon (map from edges)

-- | a === b = from a == to b && to a == from b
(===) :: Edge -> Edge -> Bool
a === b = from a == to b && to a == from b

isHorizontal :: Edge -> Bool
isHorizontal (edgeAngle -> x) = x == 0 || x == - pi

edgeAngle :: Edge -> Angle
edgeAngle (Edge from to) = foldAngle $ toAngle (to -~ from)

-- | reverses the Edge
swap :: Edge -> Edge
swap (Edge from to) = Edge to from

edgeLength :: Edge -> Double
edgeLength (Edge from to) = len (to - from)


-- * transformations

-- | merges polygons, if the results are konvex polygons
mergePolys :: (Edge -> Bool) -> [[Edge]] -> [[Edge]]
mergePolys p = mergePairs (maybeMergePoly p)

maybeMergePoly :: (Edge -> Bool) -> [Edge] -> [Edge] -> Maybe [Edge]
maybeMergePoly p a b = case intersectionCandidates a b of
        [] -> Nothing
        -- puts the two polygons together
        [intersectingEdge] ->
            if p intersectingEdge && isConvex candidate then Just candidate else Nothing
          where
            candidate = removeUnneededCorners (aBeforeEdge ++ bAfterEdge ++ bBeforeEdge ++ aAfterEdge)
            aBeforeEdge = takeWhile (/= intersectingEdge) a
            aAfterEdge = case dropWhile (/= intersectingEdge) a of
                (_ : tail) -> tail
            bBeforeEdge = takeWhile (/= swap intersectingEdge) b
            bAfterEdge = case dropWhile (/= swap intersectingEdge) b of
                (_ : tail) -> tail
                x -> es "ccc" (intersectingEdge `elem` a, swap intersectingEdge `elem` b)
        x -> trace "more than one intersecting edge" Nothing

-- | returns candidates the intersecting Edges where polygons can be merged
intersectionCandidates :: [Edge] -> [Edge] -> [Edge]
intersectionCandidates a b = map fst $ filter (uncurry (===)) $ cartesian a b

isConvex :: [Edge] -> Bool
isConvex =
    adjacentCyclic >>>
    map (uncurry (withView edgeAngle (-))) >>>
    map (foldToRange (0, 2 * pi)) >>>
    sum >>>
    rightAngularSum
  where
    rightAngularSum :: Angle -> Bool
    rightAngularSum a =
        case properFraction (a / (2 * pi)) of
            (1, 0) -> True
            (n, 0) -> False
            (n, x) | x < 0.00001 -> False
            x -> es "Physics.Chipmunk.OptimizePolygons.isConvex" x


-- * wedges

-- | adds wedges to polygons that overlap with other polygons to prevent moving shapes from being stuck
addWedges :: [[Edge]] -> [[Edge]]
addWedges polys =
    polys ++ catMaybes (map mWedge allEdges)
  where
    -- edges from all polygons flattened
    allEdges :: [Edge]
    allEdges = concat polys
    mWedge :: Edge -> Maybe [Edge]
    mWedge a =
        case filter (\ e -> to a == from e && edgeAngle a == edgeAngle e) allEdges of
            [] -> Nothing
            -- found at least one fitting edge, therefore adding a wedge
            (e : _) -> Just $ toEdges $ Polygon [newPoint id a e, newPoint negate (swap e) (swap a), to a]
    newPoint angleSign a b = to a +~ scale (fromAngle angle) len
      where
        len = edgeLength b - epsilon
        angle = edgeAngle a - angleSign (asin (epsilon / len))
    epsilon = 1


-- * removing unneeded corners of polygons

removeUnneededCorners :: [Edge] -> [Edge]
removeUnneededCorners =
    mergeAdjacentCyclicPairs merge
  where
    merge :: Edge -> Edge -> Maybe Edge
    merge a b = if edgeAngle a == edgeAngle b then
                    Just $ Edge (from a) (to b)
                  else
                    Nothing
