{-# language ScopedTypeVariables #-}

module Top.Initialisation where


import Prelude hiding (catch)
import Safe

import qualified Data.Indexable as I
import Data.Indexable (Indexable, (>:))
import Data.Initial
import Data.SelectTree
import Data.Maybe

import Text.Logging

import Control.Monad
import Control.Monad.CatchIO
import Control.Exception (IOException)

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
import qualified Sorts.DeathStones
import qualified Sorts.Box
import qualified Sorts.Battery
import qualified Sorts.Grids
import qualified Sorts.Switch
import qualified Sorts.Background
import qualified Sorts.LowerLimit

import qualified Sorts.Robots.Jetpack
import qualified Sorts.Robots.PathRobots
import qualified Sorts.Robots.Cannon
import qualified Sorts.Robots.Laser

-- import qualified Sorts.DebugObject

-- sort loaders are given individually to be able
-- to surround every single loader with an exception
-- handler.
sortLoaders :: [RM (Maybe Sort_)]
sortLoaders =

    Sorts.Tiles.sorts ++

    Sorts.Robots.Jetpack.sorts ++
    Sorts.Robots.PathRobots.sorts ++
    Sorts.Robots.Cannon.sorts ++
    Sorts.Robots.Laser.sorts ++

    Sorts.Terminal.sorts ++

    Sorts.Battery.sorts ++

    Sorts.Switch.sorts ++

    Sorts.Sign.sorts ++
    Sorts.Box.sorts ++
    Sorts.FallingTiles.sorts ++
    Sorts.DeathStones.sorts ++
    Sorts.LowerLimit.sorts ++

    Sorts.Background.sorts ++
    Sorts.Grids.sorts ++

    Sorts.Nikki.sorts ++
--     Sorts.DebugObject.sorts :
    []

withAllSorts :: (SelectTree Sort_ -> RM a) -> RM a
withAllSorts cmd = do
    sorts <- getAllSorts
    cmd sorts `finally` (io $ freeAllSorts sorts)

-- | returns all sorts in a nicely sorted SelectTree
getAllSorts :: RM (SelectTree Sort_)
getAllSorts = do
    sorts <- catMaybes <$> mapM catchExceptions sortLoaders
    io $ checkUniqueSortIds sorts
    return $ mkSortsSelectTree sorts
  where
    catchExceptions :: RM (Maybe a) -> RM (Maybe a)
    catchExceptions action =
        catch action $ \ (e :: IOException) -> io $ do
            logg Error ("cannot load all sorts: " ++ show e)
            return Nothing

checkUniqueSortIds :: [Sort_] -> IO ()
checkUniqueSortIds sorts =
    when (not $ null $ ds) $
        fail ("duplicate sort ids found: " ++ unwords ds)
  where
    ds = duplicates $ map (getSortId . sortId) sorts


freeAllSorts :: SelectTree Sort_ -> IO ()
freeAllSorts sorts = do
    fmapM_ freeSort sorts


initScene :: Application -> LevelFile -> Space -> CachedTiles
    -> Grounds (EditorObject Sort_) -> RM (Scene Object_)
initScene app file space cachedTiles =
    return . (mainLayer .> content ^: RenderOrdering.sortMainLayer) >=>
    return . groundsMergeLayers >=>
    return . groundsMergeTiles >=>
    initializeObjects app file space cachedTiles >=>
    io . mkScene file space >=>
    return . Sorts.LowerLimit.promoteLowerLimit

initializeObjects :: Application -> LevelFile -> Space -> CachedTiles
    -> Grounds (EditorObject Sort_) -> RM (Grounds Object_)
initializeObjects app file space cachedTiles (Grounds backgrounds mainLayer foregrounds) = do
    bgs' <- fmapM (fmapM (editorObject2Object app file Nothing cachedTiles)) backgrounds
    ml' <- fmapM (editorObject2Object app file (Just space) cachedTiles) mainLayer
    fgs' <- fmapM (fmapM (editorObject2Object app file Nothing cachedTiles)) foregrounds
    return $ Grounds bgs' ml' fgs'

editorObject2Object :: Application -> LevelFile -> Maybe Space -> CachedTiles
    -> EditorObject Sort_ -> RM Object_
editorObject2Object app file mspace cachedTiles (EditorObject sort pos state) =
    initialize app file mspace sort pos state cachedTiles

mkScene :: LevelFile -> Space -> Grounds Object_ -> IO (Scene Object_)
mkScene levelFile space objects = do
    contactRef <- initContactRef space initial watchedContacts
    let nikki = Sorts.Nikki.searchNikki objects
        optObjects = mkGameGrounds objects
        totalSwitches = Sorts.Switch.countSwitches (objects ^. mainLayer ^. content)
        totalBatteries =
            fromIntegral $
            Sorts.Battery.countBatteries $
            fmap sort_ $
            (objects ^. mainLayer ^. content)
    return $ Scene levelFile 0 optObjects Nothing
                (0 :!: totalBatteries)
                (0 :!: totalSwitches)
                contactRef
        initial (NikkiMode nikki)

groundsMergeTiles :: Grounds (EditorObject Sort_) -> Grounds (EditorObject Sort_)
groundsMergeTiles =
    (backgrounds ^: fmap (content ^: mergeEditorObjects)) .
    (mainLayer .> content ^: mergeEditorObjects) .
    (foregrounds ^: fmap (content ^: mergeEditorObjects))

mergeEditorObjects :: Indexable (EditorObject Sort_) -> Indexable (EditorObject Sort_)
mergeEditorObjects ixs =
    otherObjects >: Sorts.Tiles.mkAllTiles (I.toList ixs)
  where
    otherObjects = I.filter (not . isTileSort . editorSort) ixs


-- | Merge consecutive foreground and background layers
-- when it wouldn't change the rendering. Allows for more baking.
-- Conditions:
--      Both layers must have the same layer distance.
--      The lower layer mustn't have non-tile objects above tile objects.
--      The upper layer mustn't have non-tile objects below tile objects.
-- Indexes of the multilayers will be lost.
groundsMergeLayers :: Grounds (EditorObject Sort_) -> Grounds (EditorObject Sort_)
groundsMergeLayers =
    (backgrounds ^: mergeLayers) >>>
    (foregrounds ^: mergeLayers)
  where
    mergeLayers :: Indexable (Layer (EditorObject Sort_))
        -> Indexable (Layer (EditorObject Sort_))
    mergeLayers = ftoList >>> it >>> I.fromList
    it :: [Layer (EditorObject Sort_)] -> [Layer (EditorObject Sort_)]
    it (a : b : r) = case merge a b of
        Just n -> it (n : r)
        Nothing -> a : it (b : r)
    it x = x
    merge :: Layer (EditorObject Sort_) -> Layer (EditorObject Sort_)
        -> Maybe (Layer (EditorObject Sort_))
    merge a b =
      if sameDistance a b && tilesOnTop a && tilesOnBottom b
        then Just $ concatLayers a b
        else Nothing

    concatLayers a b = Layer {
        content_ = I.append (a ^. content) (ftoList (b ^. content)),
        xDistance = xDistance a,
        yDistance = yDistance a
      }

    sameDistance a b = xDistance a == xDistance b && yDistance a == yDistance b

    tilesOnTop, tilesOnBottom :: Layer (EditorObject Sort_) -> Bool
    tilesOnTop l = maybe False (isTileSort . editorSort) (lastMay $ ftoList (l ^. content))
    tilesOnBottom l = maybe False (isTileSort . editorSort) (headMay $ ftoList (l ^. content))
