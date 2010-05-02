{-# language NamedFieldPuns #-}

module Game.OptimizeChipmunks (optimizeChipmunks) where


import Utils

import Data.Indexable

import Physics.Chipmunk

import Graphics.Qt

import Game.Scene
import Game.Scene.Grounds
import Game.Objects

import Editor.Sprited


optimizeChipmunks :: UninitializedScene -> IO UninitializedScene
optimizeChipmunks scene@Scene{objects} =
    return scene{objects = objects'}
  where
    objects' = modifyMainLayer (optimizeMerge opt) objects

opt :: UninitializedObject -> UninitializedObject -> Maybe UninitializedObject
opt a@Tile{} b@Tile{} =
    if canBeMerged a b then
        let anchor = chipmunk a
        in Just $ MergedTile (map (mkMergedSprited anchor) [a, b]) anchor
      else
        Nothing
opt a@Tile{} (MergedTile merged anchor) =
    if oneCanBeMerged then
        Just $ MergedTile (merged +: mkMergedSprited anchor a) anchor
      else
        Nothing
  where
    oneCanBeMerged =
        any (canBeMerged a) vectorSpriteds
    vectorSpriteds = map (mkTempTile anchor) merged
opt MergedTile{} Tile{} = e "cooo"
opt a@MergedTile{} b@MergedTile{} =
    if oneCanBeMerged then
        Just $ MergedTile (merged a ++ map (mkMergedSprited newAnchor) bVectorSpriteds) newAnchor
      else
        Nothing
  where
    oneCanBeMerged =
        any (\ a -> any (\ b -> canBeMerged a b) bVectorSpriteds) aVectorSpriteds
    aVectorSpriteds = map (mkTempTile aAnchor) $ merged a
    bVectorSpriteds = map (mkTempTile bAnchor) $ merged b
    aAnchor = chipmunk a
    bAnchor = chipmunk b
    newAnchor = chipmunk a
opt _ _ = Nothing


-- | looks, if two Spriteds can be merged (actually just horizontally adjacent)
canBeMerged :: UninitializedObject -> UninitializedObject -> Bool
canBeMerged a b = horizontallyAdjacent || verticallyAdjacent
  where
    horizontallyAdjacent = sameHeight && sameY && xAdjacent
    sameHeight = withView (height . defaultPixmapSize . sprited) (==) a b
    sameY = withView (vectorY . chipmunk) (==) a b
    xAdjacent = xDist == searchedXDist
    xDist = withView (vectorX . chipmunk) distance a b
    searchedXDist = withView ((/ 2) . width . defaultPixmapSize . sprited) (+) a b

    verticallyAdjacent = sameWidth && sameX && yAdjacent
    sameWidth = withView (width . defaultPixmapSize . sprited) (==) a b
    sameX = withView (vectorX . chipmunk) (==) a b
    yAdjacent = yDist == searchedYDist
    yDist = withView (vectorY . chipmunk) distance a b
    searchedYDist = withView ((/ 2) . height . defaultPixmapSize . sprited) (+) a b


mkTempTile :: Vector -> MergedSprited -> UninitializedObject
mkTempTile anchor (MergedSprited sprited offset animation) =
    Tile sprited (anchor + positionToVector offset) animation

-- | makes a MergedSprited to a given offset
mkMergedSprited :: Vector -> UninitializedObject -> MergedSprited
mkMergedSprited anchor o =
    MergedSprited (sprited o) offset (animation o)
  where
    offset = vectorToPosition (chipmunk o - anchor)


