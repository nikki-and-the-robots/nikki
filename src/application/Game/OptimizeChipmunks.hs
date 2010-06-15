{-# language NamedFieldPuns, ScopedTypeVariables #-}

module Game.OptimizeChipmunks (optimizeEditorObjects) where


import Data.Indexable

import Utils

import Object

import qualified Sorts.Tiles as Tiles


optimizeEditorObjects :: Indexable EditorObject -> Indexable EditorObject
optimizeEditorObjects = optimizeMerge opt

opt :: EditorObject -> EditorObject -> Maybe EditorObject
opt a b | optIsTile a && optIsTile b =
    merge Tiles.canBeMerged a b
opt _ _ = Nothing

-- | returns True if the object is a Tile or an MergedTilesEditorObject
optIsTile :: EditorObject -> Bool
optIsTile EditorObject{editorSort} = isTile editorSort
optIsTile MergedTilesEditorObject{} = True

merge :: (EditorObject -> EditorObject -> Bool)
    -> EditorObject -> EditorObject -> Maybe EditorObject
merge canBeMerged a@EditorObject{} b@EditorObject{} =
    if canBeMerged a b then
-- --         let anchor = editorPosition a
        Just $ MergedTilesEditorObject [a, b]
-- --         (map (mkMergedSprited anchor) [a, b]) anchor
      else
        Nothing
merge canBeMerged a@EditorObject{} (MergedTilesEditorObject merged) =
    if any (canBeMerged a) merged then
        Just $ MergedTilesEditorObject (merged +: a)
      else
        Nothing
  where
--     oneCanBeMerged =
--         any (canBeMerged a) vectorSpriteds
--     vectorSpriteds = map (mkTempTile anchor) merged
merge p a@MergedTilesEditorObject{} b@EditorObject{} = merge p b a
merge canBeMerged (MergedTilesEditorObject aa) (MergedTilesEditorObject bb) =
    if oneCanBeMerged then
--         Just $ MergedTile (merged a ++ map (mkMergedSprited newAnchor) bVectorSpriteds) newAnchor
        Just $ MergedTilesEditorObject (aa ++ bb)
      else
        Nothing
  where
    oneCanBeMerged =
        any (\ a -> any (\ b -> canBeMerged a b) bb) aa
--     aVectorSpriteds = map (mkTempTile aAnchor) $ merged a
--     bVectorSpriteds = map (mkTempTile bAnchor) $ merged b
--     aAnchor = Object.position a
--     bAnchor = Object.position b
--     newAnchor = Object.position a
-- opt _ _ = Nothing
-- 
-- 

merge _ a b = es "merge" (a, b)
