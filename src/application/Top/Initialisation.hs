
module Top.Initialisation where


import Data.Indexable as I

import Physics.Chipmunk

import Utils

import Base.Grounds

import Object.Types
import Object.Contacts

import Game.Scene.Types
import Game.Scene.Camera

import qualified Sorts.Nikki
import qualified Sorts.Terminal
import qualified Sorts.Tiles
import qualified Sorts.Robots.Jetpack


sortLoaders :: [IO [Sort_]]
sortLoaders = [
    Sorts.Nikki.sorts,
    Sorts.Terminal.sorts,
    Sorts.Robots.Jetpack.sorts,
    Sorts.Tiles.sorts
  ]

mkScene :: Space -> Grounds Object_ -> IO Scene
mkScene space objects = do
    let nikki = single "savedToScene" $ I.findIndices (isNikki . sort_) $ mainLayerIndexable objects
    contactRef <- initContactRef space emptyContacts watchedContacts
    let contacts = (contactRef, emptyContacts)
    return $ Scene 0 0 objects initialCameraState contacts (NikkiMode nikki)

