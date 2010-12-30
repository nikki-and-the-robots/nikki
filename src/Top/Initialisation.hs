
module Top.Initialisation where


import qualified Data.Indexable as I
import Data.Indexable (Index, Indexable, (>:))
import Data.Initial
import Data.SelectTree

import Control.Exception
import Control.Monad

import Physics.Chipmunk

import Utils

import Base

import Object

import qualified Editor.Scene.RenderOrdering as RenderOrdering

import qualified Sorts.Nikki
import qualified Sorts.Terminal
import qualified Sorts.Tiles
import qualified Sorts.FallingTiles
import qualified Sorts.Box
import qualified Sorts.Battery
import qualified Sorts.Grids
import qualified Sorts.Switch
import qualified Sorts.Background

import qualified Sorts.Robots.Jetpack
import qualified Sorts.Robots.MovingPlatforms

import qualified Sorts.DebugObject


sortLoaders :: [IO [Sort_]]
sortLoaders =
    Sorts.Nikki.sorts :

    Sorts.Tiles.sorts :
    Sorts.FallingTiles.sorts :
    Sorts.Switch.sorts :
    Sorts.Terminal.sorts :
    Sorts.Battery.sorts :
    Sorts.Box.sorts :

    Sorts.Robots.Jetpack.sorts :
    Sorts.Robots.MovingPlatforms.sorts :

    Sorts.Grids.sorts :
    Sorts.Background.sorts :
    Sorts.DebugObject.sorts :
    []

withAllSorts :: (SelectTree Sort_ -> IO a) -> IO a
withAllSorts cmd = do
    sorts <- getAllSorts
    cmd sorts `finally` freeAllSorts sorts

-- | returns all sorts in a nicely sorted SelectTree
getAllSorts :: IO (SelectTree Sort_)
getAllSorts = do
    sorts <- concat <$> mapM id sortLoaders
    checkUniqueSortIds sorts
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

checkUniqueSortIds :: [Sort_] -> IO ()
checkUniqueSortIds sorts =
    when (not $ null $ ds) $
        fail ("duplicate sort ids found: " ++ unwords ds)
  where
    ds = duplicates $ map (getSortId . sortId) sorts


freeAllSorts :: SelectTree Sort_ -> IO ()
freeAllSorts sorts = do
    fmapM_ freeSort sorts


initScene :: Space -> Grounds (EditorObject Sort_) -> IO (Scene Object_)
initScene space =
    fromPure (modifyMainLayer RenderOrdering.sortMainLayer) >>>>
    fromPure groundsMergeTiles >>>>
    fromPure selectNikki >>>>
    secondKleisli (initializeObjects space) >>>>
    mkScene space

-- | select the last set nikki and delete all duplicates
selectNikki :: Grounds (EditorObject Sort_) -> (Index, Grounds (EditorObject Sort_))
selectNikki objects = (nikki, modifyMainLayer deleteDuplicateNikkis objects)
  where
    nikkiIndices = I.findIndices (isNikki . editorSort) $ mainLayerIndexable objects
    nikki = case nikkiIndices of
                    [a] -> a
                    (_ : _) -> trace "Warning, level containing more than one Nikki" $
                               last nikkiIndices
                    [] -> error "no Nikki found"
    -- delete duplicate nikkis
    deleteDuplicateNikkis layer =
        foldr I.deleteByIndex layer (filter (/= nikki) nikkiIndices)

initializeObjects :: Space -> Grounds (EditorObject Sort_) -> IO (Grounds Object_)
initializeObjects space (Grounds backgrounds mainLayer foregrounds) = do
    bgs' <- fmapM (fmapM (editorObject2Object Nothing)) backgrounds
    ml' <- fmapM (editorObject2Object (Just space)) mainLayer
    fgs' <- fmapM (fmapM (editorObject2Object Nothing)) foregrounds
    return $ Grounds bgs' ml' fgs'

editorObject2Object :: Maybe Space -> EditorObject Sort_ -> IO Object_
editorObject2Object mspace (EditorObject sort pos state) =
    initialize sort mspace pos (fmap oemState state)

mkScene :: Space -> (Index, Grounds Object_) -> IO (Scene Object_)
mkScene space (nikki, objects) = do
    contactRef <- initContactRef space initial watchedContacts
    return $ Scene 0 objects contactRef initial (NikkiMode nikki)

groundsMergeTiles :: Grounds (EditorObject Sort_) -> Grounds (EditorObject Sort_)
groundsMergeTiles =
    modifyMainLayer mergeEditorObjects

mergeEditorObjects :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
mergeEditorObjects ixs =
    otherObjects >: Sorts.Tiles.mkAllTiles tiles
  where
    tiles = I.toList $ I.filter (isTile . editorSort) ixs
    otherObjects = I.filter (not . isTile . editorSort) ixs
