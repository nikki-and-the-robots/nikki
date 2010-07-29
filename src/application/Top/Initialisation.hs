
module Top.Initialisation where


import Data.Indexable as I
import Data.Initial

import Physics.Chipmunk

import Utils

import Base.Grounds
import Base.Types

import Object

import Game.OptimizeChipmunks

import qualified Sorts.Nikki
import qualified Sorts.Terminal
import qualified Sorts.Tiles
import qualified Sorts.FallingTiles
import qualified Sorts.Box
import qualified Sorts.Battery
import qualified Sorts.Grids

import qualified Sorts.Robots.Jetpack


sortLoaders :: [IO [Sort_]]
sortLoaders = [
    Sorts.Nikki.sorts,
    Sorts.Tiles.sorts,

    Sorts.Terminal.sorts,
    Sorts.Battery.sorts,

    Sorts.Robots.Jetpack.sorts,

    Sorts.Box.sorts,
    Sorts.FallingTiles.sorts,
    Sorts.Grids.sorts
  ]


initSceneFromEditor :: Space -> Grounds EditorObject -> IO (Scene Object_)
initSceneFromEditor space =
    fromPure groundsOptimizeChipmunks >>>>
    initializeObjects space >>>>
    mkScene space

initializeObjects :: Space -> Grounds EditorObject -> IO (Grounds Object_)
initializeObjects space (Grounds backgrounds mainLayer foregrounds) = do
    bgs' <- fmapM (fmapM (editorObject2Object Nothing)) backgrounds
    ml' <- fmapM (editorObject2Object (Just space)) mainLayer
    fgs' <- fmapM (fmapM (editorObject2Object Nothing)) foregrounds
    return $ Grounds bgs' ml' fgs'

editorObject2Object :: Maybe Space -> EditorObject -> IO Object_
editorObject2Object mspace (EditorObject sort pos state) =
    initialize sort mspace pos (fmap oemState state)
editorObject2Object (Just space) (MergedTilesEditorObject merged) =
    Sorts.Tiles.initializeMerged space merged



mkScene :: Space -> Grounds Object_ -> IO (Scene Object_)
mkScene space objects = do
    let nikki = single "savedToScene" $ I.findIndices (isNikki . sort_) $ mainLayerIndexable objects
    contactRef <- initContactRef space initial watchedContacts
    return $ Scene 0 0 objects initial contactRef initial (NikkiMode nikki)

groundsOptimizeChipmunks :: Grounds EditorObject -> Grounds EditorObject
groundsOptimizeChipmunks =
    modifyMainLayer optimizeEditorObjects




