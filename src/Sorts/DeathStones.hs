{-# language ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.DeathStones (sorts, animationFrameTime) where


import Data.Abelian
import Data.Typeable

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base


-- * configuration

type StoneDescription = (SortId, [String], Offset Double, Size Double)

laserDir = "deathstones" </> "lasers"

stones :: [StoneDescription]
stones =
    -- horizontal
    (SortId "deathstones/lasers/horizontal",
     (laserDir </> "laser-horizontal_00") :
        (laserDir </> "laser-horizontal_01") :
        [],
     Position 1 17,
     fmap fromUber $ Size 8 5) :
    (SortId "deathstones/lasers/horizontal-small",
     (laserDir </> "laser-horizontal-small_00") :
        (laserDir </> "laser-horizontal-small_01") :
        [],
     Position 1 17,
     fmap fromUber $ Size 1 5) :

    -- vertical
    (SortId "deathstones/lasers/vertical",
     (laserDir </> "laser-vertical_00") :
        (laserDir </> "laser-vertical_01") :
        [],
     Position 17 1,
     fmap fromUber $ Size 5 8) :
    (SortId "deathstones/lasers/vertical-small",
     (laserDir </> "laser-vertical-small_00") :
        (laserDir </> "laser-vertical-small_01") :
        [],
     Position 17 1,
     fmap fromUber $ Size 5 1) :

    []

animationFrameTime :: Seconds = 0.1


-- * loading

sorts :: [RM (Maybe Sort_)]
sorts = map ((Just <$>) . loadStone) stones

loadStone :: StoneDescription -> RM Sort_
loadStone (sortId, imageNames, offset, size) = do
    images <- mapM (loadPixmap offset size) =<< mapM mkPngFile imageNames
    return $ Sort_ $ SSort sortId images (mkAnimation images [animationFrameTime])
  where
    mkPngFile :: String -> RM FilePath
    mkPngFile imageName = getDataFileName (pngDir </> imageName <.> "png")

data SSort = SSort {
    sortId :: SortId,
    pixmaps :: [Pixmap],
    animation :: Animation Pixmap
  }
    deriving (Show, Typeable)

data Stone = Stone {
    chipmunk :: Chipmunk
  }
    deriving (Show, Typeable)


instance Sort SSort Stone where
    sortId (SSort s _ _) = s

    size (SSort _ pixmaps _) = pixmapSize $ head pixmaps

    renderIconified sort ptr =
        renderPixmapSimple ptr $ head $ pixmaps sort

    initialize app _ Nothing sort editorPosition Nothing _ = io $ do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            position = epToPosition (size sort) editorPosition
        return $ Stone $ ImmutableChipmunk position 0 baryCenterOffset []
    initialize app _ (Just space) sort editorPosition Nothing _ = io $ do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            shapesWithAttributes = map (mkShapeDescription shapeAttributes) shapes
            position = position2vector (epToPosition (size sort) editorPosition)
                            +~ baryCenterOffset
            bodyAttributes = StaticBodyAttributes position
        chip <- CM.initChipmunk space bodyAttributes shapesWithAttributes baryCenterOffset
        return $ Stone chip

    immutableCopy (Stone x) = CM.immutableCopy x >>= return . Stone

    chipmunks (Stone x) = singleton x

    isUpdating = const False

    renderObject _ _ o sort ptr offset now = do
        (pos, _) <- getRenderPositionAndAngle (chipmunk o)
        let pixmap = pickAnimationFrame (animation sort) now
        return $ singleton $ RenderPixmap pixmap pos Nothing



-- * physics initialisation

shapeAttributes = ShapeAttributes {
    elasticity = 0,
    friction = 0,
    collisionType = DeadlyPermeableCT
  }


mkShapes :: Size Double -> ([ShapeType], Vector)
mkShapes size = ([box], baryCenterOffset)
  where
    box = mkRect (negateAbelian half) size
    half = size2position $ fmap (/ 2) size
    baryCenterOffset = position2vector half
