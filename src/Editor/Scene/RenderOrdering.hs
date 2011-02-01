

-- | the rendering order of the objects in the main layer is done automatically
-- via Editor.Scene.RenderOrder.sortRenderOrder.

module Editor.Scene.RenderOrdering (
    sortMainLayer,
--     sortShadowedTiles,
--     sortSorts,
  ) where


import Data.Indexable as I
import Data.List (isPrefixOf)

import Utils

import Base

import Object

import Sorts.Tiles


-- | sorts the objects in the main layer
sortMainLayer :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
sortMainLayer = sortShadowedTiles . sortSorts

-- | sorts the tiles in reading order (top to bottom, then left to right)
sortShadowedTiles :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
sortShadowedTiles = sortBy tilesReadOrdering

tilesReadOrdering :: EditorObject Sort_ -> EditorObject Sort_ -> Ordering
tilesReadOrdering a b | isTileSort (editorSort a) && isTileSort (editorSort b) =
    withView (editorY . editorPosition) compare a b `before`
    withView (editorX . editorPosition) compare a b
tilesReadOrdering _ _ = EQ

-- | combines two Orderings. The first given Ordering takes precedence over the second.
before :: Ordering -> Ordering -> Ordering
before EQ = id
before x = const x

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
