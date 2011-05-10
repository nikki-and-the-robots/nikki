

-- | the rendering order of the objects in the main layer is done automatically
-- via Editor.Scene.RenderOrder.sortRenderOrder.

module Editor.Scene.RenderOrdering (
    sortMainLayer,
--     sortShadowedTiles,
--     sortSorts,
  ) where


import Data.Indexable as I
import Data.List (isPrefixOf)

import Physics.Chipmunk

import Utils

import Base

import Sorts.Tiles


-- | sorts the objects in the main layer
sortMainLayer :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
sortMainLayer = sortShadowedTiles . sortSorts

-- | sorts the tiles in reading order (top to bottom, then left to right)
sortShadowedTiles :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
sortShadowedTiles = sortBy tilesReadOrdering

tilesReadOrdering :: EditorObject Sort_ -> EditorObject Sort_ -> Ordering
tilesReadOrdering a b | isTileSort (editorSort a) && isTileSort (editorSort b) =
    withView (^. editorPosition) tilesOrdering a b
tilesReadOrdering _ _ = EQ

-- | Orders left and up first. 45 degrees.
tilesOrdering :: EditorPosition -> EditorPosition -> Ordering
tilesOrdering a b =
    withView (ep2v >>> rot >>> vectorY) compare a b
  where
    ep2v :: EditorPosition -> Vector
    ep2v (EditorPosition x y) = Vector (realToFrac x) (realToFrac y)
    -- rotate by (- 45 degrees)
    rot :: Vector -> Vector
    rot = rotateVector (pi / 4)


-- | sorts the rendering order of the main layer objects be sorts:
-- terminals
-- Doors (don't exist yet)
-- NPCs (don't exist yet)
-- (our beloved) Nikki
-- Robots
-- Objects (e.g. Boxes)
-- Switches
-- Falling Tiles
-- Tiles
-- Batteries
-- Laser (without Robots)
sortSorts :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
sortSorts = sortBy (withView (getSortId . sortId . editorSort) renderOrdering)

renderOrdering :: FilePath -> FilePath -> Ordering
renderOrdering a b =
    fromPredList sortPreds a b

sortPreds :: [FilePath -> Bool]
sortPreds = map isPrefixOf (
    "background/" :
    "grid/" :
    "terminal" :
    "door" :
    "npc" :
    "nikki" :
    "robots/" :
    "objects/" :
    "switch/" :
    "fallingTile/" :
    "tiles/" :
    "battery" :
    [])

-- | turns a list of predicates into an ordering function.
-- The earlier in the list a predicate returns true, the lesser the rated item
fromPredList :: [a -> Bool] -> a -> a -> Ordering
fromPredList (pred : r) a b =
    case (pred a, pred b) of
        (True, False) -> LT
        (False, True) -> GT
        _ -> fromPredList r a b
fromPredList [] a b = EQ
