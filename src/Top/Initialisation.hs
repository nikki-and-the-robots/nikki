
module Top.Initialisation where


import qualified Data.Indexable as I
import Data.Indexable (Index, Indexable, (>:))
import Data.Initial
import Data.SelectTree

import Control.Monad.CatchIO
import Control.Monad

import Physics.Chipmunk

import Utils

import Base

import Object

import qualified Editor.Scene.RenderOrdering as RenderOrdering
import Sorts.Tiles (isTileSort)

import qualified Sorts.Nikki
import qualified Sorts.Terminal
import qualified Sorts.Tiles
import qualified Sorts.Sign
import qualified Sorts.FallingTiles
import qualified Sorts.Box
import qualified Sorts.Battery
import qualified Sorts.Grids
import qualified Sorts.Switch
import qualified Sorts.Background
import qualified Sorts.LowerLimit

import qualified Sorts.Robots.Jetpack
import qualified Sorts.Robots.MovingPlatform

-- import qualified Sorts.DebugObject


sortLoaders :: [RM [Sort_]]
sortLoaders =

    Sorts.Tiles.sorts :

    Sorts.Robots.Jetpack.sorts :
    Sorts.Robots.MovingPlatform.sorts :

    Sorts.Terminal.sorts :

    Sorts.Battery.sorts :

    Sorts.Switch.sorts :

    Sorts.Sign.sorts :
    Sorts.Box.sorts :
    Sorts.FallingTiles.sorts :
    Sorts.LowerLimit.sorts :

    Sorts.Background.sorts :
    Sorts.Grids.sorts :

    Sorts.Nikki.sorts :
--     Sorts.DebugObject.sorts :
    []

withAllSorts :: (SelectTree Sort_ -> RM a) -> RM a
withAllSorts cmd = do
    sorts <- getAllSorts
    cmd sorts `finally` (io $ freeAllSorts sorts)

-- | returns all sorts in a nicely sorted SelectTree
getAllSorts :: RM (SelectTree Sort_)
getAllSorts = do
    sorts <- concat <$> mapM id sortLoaders
    io $ checkUniqueSortIds sorts
    return $ mkSortsSelectTree sorts
  where

checkUniqueSortIds :: [Sort_] -> IO ()
checkUniqueSortIds sorts =
    when (not $ null $ ds) $
        fail ("duplicate sort ids found: " ++ unwords ds)
  where
    ds = duplicates $ map (getSortId . sortId) sorts


freeAllSorts :: SelectTree Sort_ -> IO ()
freeAllSorts sorts = do
    fmapM_ freeSort sorts


initScene :: Application -> LevelFile -> Space -> Grounds (EditorObject Sort_) -> IO (Scene Object_)
initScene app levelFile space =
    return . Sorts.Nikki.uniqueNikki app >=>
    secondKleisli (
        return . (mainLayer .> content ^: RenderOrdering.sortMainLayer) >=>
        return . groundsMergeTiles >=>
        initializeObjects app space) >=>
    mkScene levelFile space >=>
    return . Sorts.LowerLimit.promoteLowerLimit

initializeObjects :: Application -> Space -> Grounds (EditorObject Sort_) -> IO (Grounds Object_)
initializeObjects app space (Grounds backgrounds mainLayer foregrounds) = do
    bgs' <- fmapM (fmapM (editorObject2Object app Nothing)) backgrounds
    ml' <- fmapM (editorObject2Object app (Just space)) mainLayer
    fgs' <- fmapM (fmapM (editorObject2Object app Nothing)) foregrounds
    return $ Grounds bgs' ml' fgs'

editorObject2Object :: Application -> Maybe Space -> EditorObject Sort_ -> IO Object_
editorObject2Object app mspace (EditorObject sort pos state) =
    initialize app mspace sort pos state

mkScene :: LevelFile -> Space -> (Index, Grounds Object_) -> IO (Scene Object_)
mkScene levelFile space (nikki, objects) = do
    contactRef <- initContactRef space initial watchedContacts
    let optObjects = mkGameGrounds objects
        totalSwitches = Sorts.Switch.countSwitches (objects ^. mainLayer ^. content)
    return $ Scene levelFile 0 optObjects Nothing 0 (0, totalSwitches) contactRef
        initial (NikkiMode nikki)

groundsMergeTiles :: Grounds (EditorObject Sort_) -> Grounds (EditorObject Sort_)
groundsMergeTiles =
    mainLayer .> content ^: mergeEditorObjects

mergeEditorObjects :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
mergeEditorObjects ixs =
    otherObjects >: Sorts.Tiles.mkAllTiles tiles
  where
    tiles = I.toList $ I.filter (isTileSort . editorSort) ixs
    otherObjects = I.filter (not . isTileSort . editorSort) ixs
