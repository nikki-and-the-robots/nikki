{-# language NamedFieldPuns, ScopedTypeVariables #-}

module Game.OptimizeChipmunks (optimizeEditorObjects) where


import Data.Indexable as I

import Utils

import Base.Types

import Object

import qualified Sorts.Tiles as Tiles


optimizeEditorObjects :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
optimizeEditorObjects ixs =
    otherObjects >: Tiles.mkAllTiles tiles
  where
    tiles = toList $ I.filter (isTile . editorSort) ixs
    otherObjects = I.filter (not . isTile . editorSort) ixs
