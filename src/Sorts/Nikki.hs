{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
    FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Sorts.Nikki (sorts, addBatteryPower, modifyNikki, nikkiMass, walkingVelocity) where


import Prelude hiding (lookup)

import Data.Map (Map, toList, fromList, (!), lookup)
import Data.Abelian
import Data.Generics
import Data.Initial
import Data.Foldable
import Data.Buffer
import Data.Maybe

import System.FilePath

import Graphics.Qt as Qt hiding (rotate, scale)

import Sound.SFML

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)
import qualified Physics.Hipmunk as Hip

import Utils

import Base

import Object

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration
import Sorts.Nikki.Initialisation
import Sorts.Nikki.State
import Sorts.Nikki.Control
import Sorts.Nikki.JumpingImpulse
import Sorts.Nikki.Dust


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
    fromList <$> (fmapM load $ Data.Map.toList statePixmaps)
  where
    load :: (String, Int) -> IO (String, [Pixmap])
    load (name, n) = do
        pixmaps <- mapM (getDataFileName >>>> loadPixmap nikkiPngOffset) $
                        map (mkPngPath name) [0..n]
        return (name, pixmaps)
    nikkiPngOffset = Position (1 + fromUber 3) (1 + 6)

mkPngPath name n = nikkiPngDir </> name ++ "_0" ++ show n <.> "png"

nikkiPngDir = pngDir </> "nikki"

defaultPixmap :: Map String [Pixmap] -> Pixmap
defaultPixmap pixmaps = head (pixmaps ! "wait_right")


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

    size sort = nikkiSize

    sortRender sort ptr _ =
        renderPixmapSimple ptr (defaultPixmap $ pixmaps sort)

    initialize sort (Just space) editorPosition Nothing = do
        let (surfaceVelocityShapeType, otherShapes, baryCenterOffset) = mkPolys
            pos = qtPosition2Vector (editorPosition2QtPosition sort editorPosition)
                    +~ baryCenterOffset

        chip <- CM.initChipmunk space (bodyAttributes pos) (surfaceVelocityShapeType : otherShapes)
                    baryCenterOffset

        let surfaceVelocityShape = head $ shapes chip

        return $ Nikki
            chip
            surfaceVelocityShape
            initial
            0
            0
            (mkFullBuffer 2000 (zero, zero))
            (zero, zero)

    immutableCopy n@Nikki{chipmunk} = CM.immutableCopy chipmunk >>= \ new -> return n{chipmunk = new}

    chipmunks = return . chipmunk

    getControlledChipmunk _ = chipmunk

    updateNoSceneChange sort mode now contacts cd nikki = inner nikki
      where
        inner =
            updateState mode now contacts cd >>>>
            fromPure (updateStartTime now (state nikki)) >>>>
            controlNikki now contacts cd sort >>>>
--             debugNikki now contacts >>>>
            return

    render nikki sort ptr offset now = do
        let pixmap = pickPixmap now sort nikki
        renderChipmunk ptr offset pixmap (chipmunk nikki)
        renderDustClouds ptr offset now sort (dustClouds $ state nikki)


pickPixmap :: Seconds -> NSort -> Nikki -> Pixmap
pickPixmap now sort nikki =
    let (name, frameTimes_) = frameTimes $ state nikki
        m = lookup name (pixmaps sort)
    in case m of
        Just pixmapList ->
            pickAnimationFrameNonLooping pixmapList frameTimes_ (now - startTime nikki)
        Nothing -> es "problem finding pixmaps in Nikki: " name


-- debugging

debugNikki :: Seconds -> Contacts -> Nikki -> IO Nikki
debugNikki now contacts nikki@Nikki{positionBuffer} = do
    vel <- get $ velocity $ body $ chipmunk $ nikki
    p <- getPosition (chipmunk nikki)
    addDebugging $ \ ptr offset -> do
        resetMatrix ptr
        windowSize <- fmap fromIntegral <$> sizeQPainter ptr
        translate ptr (Position (width windowSize - 2000) 0)
        forM_ positionBuffer $ \ (p, v) -> do
            setPenColor ptr blue 2
            drawPoint ptr (Position 0 (height windowSize + vectorY p / 5))
            setPenColor ptr red 2
            drawPoint ptr (Position 0 (height windowSize / 2 + vectorY v / 5))
            translate ptr (Position 1 0)
        resetMatrix ptr
        setPenColor ptr green 1
        forM_ (localMinima $ map (vectorY . fst) $ Data.Foldable.toList positionBuffer) $ \ minimum -> do
            let y = (height windowSize + minimum / 5)
            drawLine ptr (Position 0 y) (Position (width windowSize) y)
    return (if not (vectorY (fst $ lastPosition nikki) ~= (vectorY p)) then
        nikki{positionBuffer = fromJust $ enqueue (p, vel) $ snd $ fromJust $ dequeue positionBuffer}
      else
        nikki){lastPosition = (p, vel)}

scaleVector v = scale v (0.25 / nikkiMass)

drawVector :: Ptr QPainter -> Color -> Vector -> IO ()
drawVector ptr color v = do
    setPenColor ptr color 3
    drawLine ptr zero $ vector2QtPosition $ scaleVector v

drawVectorAddition :: Ptr QPainter -> (Color, Color, Color) -> Vector -> Vector -> IO ()
drawVectorAddition ptr (aColor, bColor, cColor) a b = do
    drawVector ptr aColor a
    translateVector ptr $ scaleVector a
    drawVector ptr bColor b
    translateVector ptr $ scaleVector (negateAbelian a)
    drawVector ptr cColor (a +~ b)

drawAngle ptr color angle = drawVector ptr color $ flip scale 1000000 $ fromUpAngle angle
