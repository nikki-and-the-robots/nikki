{-# language NamedFieldPuns, ScopedTypeVariables #-}

module Game.OptimizeChipmunks (optimizeEditorObjects) where


import Data.Indexable

import Utils

import Base.Types

import Object

import qualified Sorts.Tiles as Tiles


optimizeEditorObjects :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
optimizeEditorObjects = optimizeMerge opt

opt :: EditorObject Sort_ -> EditorObject Sort_ -> Maybe (EditorObject Sort_)
opt a b | optIsTile a && optIsTile b =
    merge Tiles.canBeMerged a b
opt _ _ = Nothing

-- | returns True if the object is a Tile or an MergedTilesEditorObject
optIsTile :: EditorObject Sort_ -> Bool
optIsTile EditorObject{editorSort} = isTile editorSort
optIsTile MergedTilesEditorObject{} = True

merge :: (EditorObject Sort_ -> EditorObject Sort_ -> Bool)
    -> EditorObject Sort_ -> EditorObject Sort_ -> Maybe (EditorObject Sort_)
merge canBeMerged a@EditorObject{} b@EditorObject{} =
    if canBeMerged a b then
        Just $ MergedTilesEditorObject [a, b]
      else
        Nothing
merge canBeMerged a@EditorObject{} (MergedTilesEditorObject merged) =
    if any (canBeMerged a) merged then
        Just $ MergedTilesEditorObject (merged +: a)
      else
        Nothing
merge p a@MergedTilesEditorObject{} b@EditorObject{} = merge p b a
merge canBeMerged (MergedTilesEditorObject aa) (MergedTilesEditorObject bb) =
    if oneCanBeMerged then
        Just $ MergedTilesEditorObject (aa ++ bb)
      else
        Nothing
  where
    oneCanBeMerged =
        any (\ a -> any (\ b -> canBeMerged a b) bb) aa




