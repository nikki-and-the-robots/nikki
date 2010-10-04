
module Top.Initialisation where


import Data.Indexable as I
import Data.Initial
import Data.SelectTree
import Data.List as List

import Control.Exception

import Physics.Chipmunk

import Utils

import Base.Grounds
import Base.Types

import Object

import Game.OptimizeChipmunks
import Game.Scene.Camera

import qualified Sorts.Nikki
import qualified Sorts.Terminal
import qualified Sorts.Tiles
import qualified Sorts.FallingTiles
import qualified Sorts.Box
import qualified Sorts.Battery
import qualified Sorts.Grids
import qualified Sorts.Switch

import qualified Sorts.Robots.Jetpack
import qualified Sorts.Robots.MovingPlatforms


sortLoaders :: [IO [Sort_]]
sortLoaders = [
    Sorts.Nikki.sorts,

    Sorts.Tiles.sorts,
    Sorts.FallingTiles.sorts,
    Sorts.Switch.sorts,
    Sorts.Terminal.sorts,
    Sorts.Battery.sorts,
    Sorts.Box.sorts,

    Sorts.Robots.Jetpack.sorts,
    Sorts.Robots.MovingPlatforms.sorts,

    Sorts.Grids.sorts
  ]

withAllSorts :: (SelectTree Sort_ -> IO a) -> IO a
withAllSorts cmd = do
    sorts <- getAllSorts
    cmd sorts `finally` freeAllSorts sorts

-- | returns all sorts in a nicely sorted SelectTree
getAllSorts :: IO (SelectTree Sort_)
getAllSorts = do
    sorts <- concat <$> mapM id sortLoaders
    return $ mkSelectTree sorts
  where
    mkSelectTree :: [Sort_] -> SelectTree Sort_
    mkSelectTree sorts =
        foldl (flip addSort) (EmptyNode "objects") sorts
      where
        addSort :: Sort_ -> SelectTree Sort_ -> SelectTree Sort_
        addSort sort t = addByPrefix (init $ wordsBy ['/'] (getSortId $ sortId sort)) sort t

        -- | adds an element by a given prefix to a SelectTree. If branches with needed labels
        -- are missing, they are created.
        -- PRE: The tree is not a Leaf.
        addByPrefix :: [String] -> a -> SelectTree a -> SelectTree a
        addByPrefix _ _ (Leaf _) = error "addByPrefix"
        addByPrefix (a : r) x node =
            -- prefixes left: the tree needs to be descended further
            if any (\ subTree -> getLabel subTree == Just a) (getChildren node) then
                -- if the child already exists
                modifyLabelled a (addByPrefix r x) node
              else
                -- the branch doesn't exist, it's created
                addChild (addByPrefix r x (EmptyNode a)) node
          where
            Just parentLabel = getLabel node
        addByPrefix [] x node =
            -- no prefixes left: here the element is added
            addChild (Leaf x) node

freeAllSorts :: SelectTree Sort_ -> IO ()
freeAllSorts sorts = do
    fmapM_ freeSort sorts


initScene :: Space -> Grounds (EditorObject Sort_) -> IO (Scene Object_)
initScene space =
    fromPure groundsOptimizeChipmunks >>>>
    initializeObjects space >>>>
    mkScene space

initializeObjects :: Space -> Grounds (EditorObject Sort_) -> IO (Grounds Object_)
initializeObjects space (Grounds backgrounds mainLayer foregrounds) = do
    bgs' <- fmapM (fmapM (editorObject2Object Nothing)) backgrounds
    ml' <- fmapM (editorObject2Object (Just space)) mainLayer
    fgs' <- fmapM (fmapM (editorObject2Object Nothing)) foregrounds
    return $ Grounds bgs' ml' fgs'

editorObject2Object :: Maybe Space -> EditorObject Sort_ -> IO Object_
editorObject2Object mspace (EditorObject sort pos state) =
    initialize sort mspace pos (fmap oemState state)
editorObject2Object (Just space) (MergedTilesEditorObject merged) =
    Sorts.Tiles.initializeMerged space merged



mkScene :: Space -> Grounds Object_ -> IO (Scene Object_)
mkScene space objects = do
    let nikki = single "savedToScene" $ I.findIndices (isNikki . sort_) $ mainLayerIndexable objects
    nikkiPosition <- getPosition $ getControlledChipmunk (mainLayerIndexable objects !!! nikki)
    contactRef <- initContactRef space initial watchedContacts
    return $ Scene 0 objects (initialCameraState nikkiPosition) contactRef initial (NikkiMode nikki)

groundsOptimizeChipmunks :: Grounds (EditorObject Sort_) -> Grounds (EditorObject Sort_)
groundsOptimizeChipmunks =
    modifyMainLayer optimizeEditorObjects




