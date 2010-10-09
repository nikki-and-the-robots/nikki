
module Physics.Chipmunk.OptimizePolygons (optimizePolygons) where


import Data.Abelian

import Control.Arrow

import Physics.Hipmunk hiding (isConvex)

import Utils

import Physics.Chipmunk.Types


-- | does optimization of polygons (merging, etc.)
optimizePolygons :: [ShapeType] -> [ShapeType]
optimizePolygons =
    map toEdges >>>
    mergePolys >>>
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
      else
        Polygon (map from edges)

-- | a === b = from a == to b && to a == from b
(===) :: Edge -> Edge -> Bool
a === b = from a == to b && to a == from b

edgeAngle :: Edge -> Angle
edgeAngle (Edge from to) = foldAngle $ toAngle (to -~ from)

-- | reverses the Edge
swapEdge :: Edge -> Edge
swapEdge (Edge from to) = Edge to from


-- * transformations

-- | merges polygons, if the results are konvex polygons
mergePolys :: [[Edge]] -> [[Edge]]
mergePolys = mergePairs maybeMergePoly

maybeMergePoly :: [Edge] -> [Edge] -> Maybe [Edge]
maybeMergePoly a b = case intersectionCandidates a b of
        [] -> Nothing
        -- puts the two polygons together
        [intersectingEdge] ->
            if isConvex candidate then Just candidate else Nothing
          where
            candidate = removeUnneededCorners (aBeforeEdge ++ bAfterEdge ++ bBeforeEdge ++ aAfterEdge)
            aBeforeEdge = takeWhile (/= intersectingEdge) a
            aAfterEdge = case dropWhile (/= intersectingEdge) a of
                (_ : tail) -> tail
            bBeforeEdge = takeWhile (/= swapEdge intersectingEdge) b
            bAfterEdge = case dropWhile (/= swapEdge intersectingEdge) b of
                (_ : tail) -> tail
                x -> es "ccc" (intersectingEdge `elem` a, swapEdge intersectingEdge `elem` b)
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
            n -> es "Physics.Chipmunk.OptimizePolygons.isConvex" n


-- * wedges

-- | adds wedges to polygons that overlap with other polygons to prevent moving shapes from being stuck
addWedges :: [[Edge]] -> [[Edge]]
addWedges = id


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
