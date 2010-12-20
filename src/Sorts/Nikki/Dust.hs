
-- | module for programming little dust clouds around nikki

module Sorts.Nikki.Dust where


import Data.Abelian
import Data.Map (lookup)

import Physics.Chipmunk

import Graphics.Qt

import Utils

import Base.Types
import Base.Constants
import Base.Pixmap
import Base.Directions
import Base.Animation

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration


-- | create nikki's dust
addDustClouds :: Seconds -> Nikki -> IO Nikki
addDustClouds now nikki = do
    p <- getPosition $ chipmunk nikki
    return nikki{state = newState p}
  where
    newState p = (state nikki){dustClouds = dustClouds' p}
    dustClouds' p = newClouds p ++ removeOldClouds (dustClouds $ state nikki)
    newClouds p = [newCloud p direction_]

    newCloud (Vector x y) _ =
        DustCloud now (Position x y)
--     newCloud (Vector x y) HRight =
--         DustCloud now (Position x y +~ Position (fromUber (13 / 2)) (fromUber (24 / 2)) +~ cloudRenderCorrection)
    cloudRenderCorrection = Position (- fromUber (5 / 2)) (- fromUber (5 / 2))
    direction_ = direction $ state nikki

    removeOldClouds = filter (\ c -> now - creationTime c < 4 * cloudCreationTime)



renderDustClouds :: Ptr QPainter -> Offset Double -> Seconds -> NSort -> [DustCloud] -> IO ()
renderDustClouds ptr offset now sort clouds =
    fmapM_ render clouds
  where
    render cloud = do
        let mPixmap = case Data.Map.lookup "dust" (pixmaps sort) of
                Just pixmapList -> do
                    pickLimitedAnimationFrame pixmapList cloudFrameTimes (now - creationTime cloud)
        case mPixmap of
            Just pixmap ->
                renderPixmap ptr offset (cloudPosition cloud) Nothing pixmap
            Nothing -> return ()
renderDustClouds _ _ _ _ _ = return ()
