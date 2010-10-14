{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
    FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Sorts.Nikki (sorts, addBatteryPower, modifyNikki, nikkiMass, walkingVelocity) where


import Prelude hiding (lookup)

import Data.Map (Map, fromList, toList, (!), lookup)
import qualified Data.Set as Set
import Data.Abelian
import Data.Generics
import Data.Initial

import Control.Monad
import Control.Arrow

import System.FilePath

import Graphics.Qt as Qt hiding (rotate, scale)

import Sound.SFML

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Paths_nikki
import Utils

import Base.Constants
import Base.Animation
import Base.Pixmap
import Base.Types

import Object

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration
import Sorts.Nikki.Initialisation
import Sorts.Nikki.State
import Sorts.Nikki.Control


sorts :: IO [Sort_]
sorts = do
    pixmaps <- loadPixmaps
    psize <- fmap fromIntegral <$> sizeQPixmap (pixmap $ defaultPixmap pixmaps)
    soundFile <- getDataFileName (soundDir </> "nikki/jump.wav")
    jumpSound <- newPolySound soundFile 4
    let r = NSort pixmaps jumpSound
    return [Sort_ r]

loadPixmaps :: IO (Map String [Pixmap])
loadPixmaps = do
    fromList <$> (fmapM load $ toList statePixmaps)
  where
    load :: (String, Int) -> IO (String, [Pixmap])
    load (name, n) = do
        pixmaps <- mapM (getDataFileName >>>> loadPixmap 1) $ map (mkPngPath name) [0..n]
        return (name, pixmaps)

mkPngPath name n = nikkiPngDir </> name ++ "_0" ++ show n <.> "png"

nikkiPngDir = pngDir </> "nikki"

defaultPixmap :: Map String [Pixmap] -> Pixmap
defaultPixmap pixmaps = head (pixmaps ! "wait_left")


modifyNikki :: (Nikki -> Nikki) -> Scene Object_ -> Scene Object_
modifyNikki f scene =
    modifyMainlayerObjectByIndex inner (nikki (mode scene)) scene
  where
    inner :: Object_ -> Object_
    inner (Object_ s o) =
        Object_ s' o'
      where
        Just s' = cast s
        Just castO = cast o
        o' = f castO


instance Sort NSort Nikki where

    sortId _ = SortId "nikki"

    freeSort (NSort pixmaps sound) = do
        fmapM_ (fmapM_ freePixmap) pixmaps
        freePolySound sound

    size sort = pixmapSize $ defaultPixmap $ pixmaps sort

    sortRender sort ptr _ =
        renderPixmapSimple ptr (defaultPixmap $ pixmaps sort)

    initialize sort (Just space) editorPosition Nothing = do
        let (surfaceVelocityShapeTypes, otherShapes, baryCenterOffset) = mkPolys $ size sort
            pos = qtPosition2Vector (editorPosition2QtPosition sort editorPosition)
                    +~ baryCenterOffset

        chip <- CM.initChipmunk space (bodyAttributes pos) (surfaceVelocityShapeTypes ++ otherShapes)
                    baryCenterOffset

        let surfaceVelocityShapes = take (length surfaceVelocityShapeTypes) $ shapes chip

        return $ Nikki
            chip
            surfaceVelocityShapes
            initial
            0
            0
            (const $ const $ return ())

    immutableCopy n@Nikki{chipmunk} = CM.immutableCopy chipmunk >>= \ new -> return n{chipmunk = new}

    chipmunks = return . chipmunk

    getControlledChipmunk = chipmunk

    updateNoSceneChange sort now contacts cd nikki = inner nikki
      where
        inner =
            updateState now contacts cd >>>>
            fromPure (updateStartTime now (state nikki)) >>>>
            passThrough (controlBody now contacts cd sort)
--             >>>> debugNikki contacts

    render nikki sort ptr offset now = do
        let pixmap = pickPixmap now sort nikki
        renderChipmunk ptr offset pixmap (chipmunk nikki)
        renderClouds ptr offset now sort (action $ state nikki)
        debugCmd nikki ptr offset


pickPixmap :: Seconds -> NSort -> Nikki -> Pixmap
pickPixmap now sort nikki =
    let (name, frameTimes_) = frameTimes $ state nikki
        m = lookup name (pixmaps sort)
    in case m of
        Just pixmapList ->
            pickAnimationFrameNonLooping pixmapList frameTimes_ (now - startTime nikki)
        Nothing -> es "problem finding pixmaps in Nikki: " name

renderClouds :: Ptr QPainter -> Offset Double -> Seconds -> NSort -> Action -> IO ()
renderClouds ptr offset now sort (WallSlide _ clouds) =
    mapM_ render clouds
  where
    render cloud = do
        let mPixmap = case lookup "dust" (pixmaps sort) of
                Just pixmapList -> do
                    pickLimitedAnimationFrame pixmapList cloudFrameTimes (now - creationTime cloud)
        case mPixmap of
            Just pixmap ->
                renderPixmap ptr offset (cloudPosition cloud) Nothing Nothing pixmap
            Nothing -> return ()
renderClouds _ _ _ _ _ = return ()



-- debugging

debugNikki :: Contacts -> Nikki -> IO Nikki
debugNikki contacts nikki = do
    return nikki{debugCmd = worker contacts}
  where
    worker contacts ptr offset = do
        resetMatrix ptr
        translate ptr offset
        setPenColor ptr 0 255 255 255 1
        nikkiPos <- getPosition $ chipmunk nikki
--         print $ map (-~ nikkiPos) (concatMap snd $ nikkiContacts contacts)
        let pawContacts = Set.unions $ map (paws nikkiPos) $ (nikkiContacts contacts)
        mapM_ (inner ptr) $ Set.toList pawContacts
    inner ptr (normal, Vector x y) = do
        when (normal == zero) $
            print $ Vector x y
        let pos = Position x y
            Vector xn yn = scale (normalize normal) 100
        drawCircle ptr pos 3
        drawLine ptr pos (pos +~ Position xn yn)

    paws :: CM.Position -> Collision -> Set.Set (Vector, Vector)
    paws nikkiPos (Collision normal points) = Set.fromList $ map (\ p -> (normal, p)) $ filter (pawsH normal . (-~ nikkiPos)) points
    pawsH ((toUpAngle >>> foldAngle) -> angle) v@(Vector x y) =
        (y =~= 19) && (angle > (- angleLimit)) && (angle < angleLimit)
    angleLimit = deg2rad 45
    a =~= b = abs (a - b) < eps
    eps = 1
