
module Top.Initialisation where


import Data.Indexable as I

import Control.Monad.FunctorM

import Physics.Chipmunk

import Utils

import Base.Grounds

import Object.Types
import Object.Contacts

import Game.Scene.Types
import Game.Scene.Camera
import Game.OptimizeChipmunks

import qualified Sorts.Nikki
import qualified Sorts.Terminal
import qualified Sorts.Tiles
import qualified Sorts.Box

import qualified Sorts.Robots.Jetpack


sortLoaders :: [IO [Sort_]]
sortLoaders = [
    Sorts.Nikki.sorts,
    Sorts.Box.sorts,
    Sorts.Terminal.sorts,
    Sorts.Robots.Jetpack.sorts,
    Sorts.Tiles.sorts
  ]


initSceneFromEditor :: Space -> Grounds EditorObject -> IO Scene
initSceneFromEditor space =
    initializeObjects space .>>
    mkScene space .>>
    optimizeChipmunks

initializeObjects :: Space -> Grounds EditorObject -> IO (Grounds Object_)
initializeObjects space (Grounds backgrounds mainLayer foregrounds) = do
    bgs' <- fmapM (fmapM (editorObject2Object Nothing)) backgrounds
    ml' <- fmapM (editorObject2Object (Just space)) mainLayer
    fgs' <- fmapM (fmapM (editorObject2Object Nothing)) foregrounds
    return $ Grounds bgs' ml' fgs'



mkScene :: Space -> Grounds Object_ -> IO Scene
mkScene space objects = do
    let nikki = single "savedToScene" $ I.findIndices (isNikki . sort_) $ mainLayerIndexable objects
    contactRef <- initContactRef space emptyContacts watchedContacts
    let contacts = (contactRef, emptyContacts)
    return $ Scene 0 0 objects initialCameraState contacts (NikkiMode nikki)

