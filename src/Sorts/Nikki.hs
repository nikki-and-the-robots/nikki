{-# language ViewPatterns, NamedFieldPuns, MultiParamTypeClasses #-}

module Sorts.Nikki (
    sorts,
    isNikki,
    uniqueNikki,
    searchNikki,
    modifyNikki,
    nikkiMass,
    maximumWalkingVelocity,
  ) where


import Prelude hiding (lookup)

import Data.Map (Map, toList, fromList, (!), lookup)
import Data.Abelian
import Data.Data
import Data.Initial
import Data.Indexable (indexA)
import qualified Data.Strict as Strict

import System.FilePath

import Graphics.Qt as Qt hiding (rotate, scale)

import Sound.SFML

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Utils

import Base

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration
import Sorts.Nikki.Initialisation
import Sorts.Nikki.State
import Sorts.Nikki.Control
import Sorts.Nikki.Batteries


sorts :: [RM (Maybe Sort_)]
sorts = singleton $ do
    pixmaps <- loadPixmaps
    psize <- io $ fmap fromIntegral <$> sizeQPixmap (pixmap $ defaultPixmap pixmaps)
    jumpSound <- loadSound "game/jump" 4
    batteryCollectSound <- loadSound "game/batteryCollect" 8
    let r = NSort pixmaps jumpSound batteryCollectSound
    return $ Just $ Sort_ r

loadPixmaps :: RM (Map String [Pixmap])
loadPixmaps = do
    fromList <$> (fmapM load $ Data.Map.toList statePixmaps)
  where
    load :: (String, Int) -> RM (String, [Pixmap])
    load (name, n) = do
        pixmaps <- mapM (getDataFileName >=> loadSymmetricPixmap nikkiPngOffset) $
                        map (mkPngPath name) [0..n]
        return (name, pixmaps)
    nikkiPngOffset = Position (1 + fromUber 3) (1 + 6)

mkPngPath name n = nikkiPngDir </> name ++ "_0" ++ show n <.> "png"

nikkiPngDir = pngDir </> "nikki"

defaultPixmap :: Map String [Pixmap] -> Pixmap
defaultPixmap pixmaps = head (pixmaps ! "wait_right")


-- | Modifies our beloved main character in the game scene.
-- Doesn't do anything, if the level is finished.
modifyNikki :: (Nikki -> Nikki) -> Scene Object_ -> Scene Object_
modifyNikki f scene | isGameMode (scene ^. mode) =
    objects .> gameMainLayer .> indexA (nikki (scene ^. mode)) ^:
        mod $
        scene
  where
    mod :: Object_ -> Object_
    mod (Object_ s o) =
        Object_ s' o'
      where
        Just s' = cast s
        Just castO = cast o
        o' = f castO
modifyNikki _ scene = scene


instance Sort NSort Nikki where

    sortId _ = SortId "nikki"

    freeSort (NSort pixmaps jumpSound batteryCollectSound) =
        freePolySound jumpSound >>
        freePolySound batteryCollectSound

    size sort = nikkiSize

    renderIconified sort ptr =
        renderPixmapSimple ptr (defaultPixmap $ pixmaps sort)

    initialize app _ (Just space) sort editorPosition Nothing _ = io $ do
        let (surfaceVelocityShapeType, otherShapes, baryCenterOffset) = mkPolys
            pos = position2vector (epToPosition (size sort) editorPosition)
                    +~ baryCenterOffset

        chip <- CM.initChipmunk space (bodyAttributes pos) (surfaceVelocityShapeType : otherShapes)
                    baryCenterOffset

        let surfaceVelocityShape = head $ shapes chip

        return $ Nikki chip (Strict.Just surfaceVelocityShape) initial 0
    initialize app _ Nothing sort editorPosition Nothing _ = do
        let (_, _, baryCenterOffset) = mkPolys
            position = epToPosition (size sort) editorPosition
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        return $ Nikki chip Strict.Nothing initial 0

    immutableCopy n@Nikki{chipmunk} = CM.immutableCopy chipmunk >>= \ new -> return n{chipmunk = new}

    chipmunks = return . chipmunk

    getControlledChipmunk _ = chipmunk

    isUpdating = const True

    update sort _ config _ scene now contacts cd _ nikki = do
        removeBatteries sort config contacts (batteryMap scene)
        io $ inner nikki
      where
        inner :: Nikki -> IO Nikki
        inner =
            updateState (config ^. controls) (scene ^. mode) now contacts cd >=>
            return . updateStartTime now (state nikki) >=>
            controlNikki config now contacts cd sort

    renderObject _ _ nikki sort _ _ now = do
        let pixmap = pickPixmap now sort nikki
        (pos, angle) <- getRenderPositionAndAngle (chipmunk nikki)
        return [RenderPixmap pixmap pos (Just angle)]


pickPixmap :: Seconds -> NSort -> Nikki -> Pixmap
pickPixmap now sort nikki =
    let (name, frameTimes_) = frameTimes $ state nikki
        m = lookup name (pixmaps sort)
    in case m of
        Just pixmapList ->
            pickAnimationFrameNonLooping pixmapList frameTimes_ (now - startTime nikki)
        Nothing -> es "problem finding pixmaps in Nikki: " name
