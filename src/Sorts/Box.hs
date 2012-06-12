{-# language ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.Box (sorts) where


import Data.Typeable
import Data.Abelian

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base


-- Configuration

names =
    ("box-wood-small", 125) :
    ("box-wood-large", 360) : -- 370
    []

-- loading

sorts :: [RM (Maybe Sort_)]
sorts =
    map ((Just <$>) . uncurry mkSort_) names
  where
    mkSortId name = SortId ("objects/" ++ name)
    mkSort_ name mass = do
        pngFile <- getDataFileName $ mkPath name
        pix <- loadSymmetricPixmap (Position 1 1) pngFile
        return $ Sort_ $ BSort (mkSortId name) mass pix

mkPath :: String -> FilePath
mkPath name = pngDir </> "objects" </> name <.> "png"

data BSort
    = BSort {
        sortId_ :: SortId,
        boxMass :: Mass, -- absolute mass of this BSort
        boxPixmap :: Pixmap
      }
  deriving (Show, Typeable)

data Box = Box {chipmunk :: Chipmunk}
    deriving (Show, Typeable)

instance Sort BSort Box where
    sortId = sortId_
    size = pixmapSize . boxPixmap
    renderIconified sort ptr =
        renderPixmapSimple ptr (boxPixmap sort)
    initialize app _ (Just space) sort editorPosition Nothing _ = io $ do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            shapesWithAttributes = map (mkShapeDescription shapeAttributes) shapes
            position = position2vector (epToPosition (size sort) editorPosition)
                            +~ baryCenterOffset
            bodyAttributes = mkBodyAttributes shapes position (boxMass sort)
        chip <- CM.initChipmunk space bodyAttributes
                    shapesWithAttributes baryCenterOffset
        return $ Box chip
    initialize app _ Nothing sort editorPosition Nothing _ = io $ do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            position = epToPosition (size sort) editorPosition
        return $ Box $ ImmutableChipmunk position 0 baryCenterOffset []
    immutableCopy (Box x) = CM.immutableCopy x >>= return . Box
    chipmunks b = [chipmunk b]
    isUpdating = const False
    renderObject _ _ o sort ptr offset now = do
        (pos, angle) <- getRenderPositionAndAngle (chipmunk o)
        return [RenderPixmap (boxPixmap sort) pos (Just angle)]

shapeAttributes :: ShapeAttributes
shapeAttributes = ShapeAttributes {
    elasticity    = 0.5,
    friction      = 0.95,
    collisionType = TileCT
  }

mkShapes :: Size Double -> ([ShapeType], Vector)
mkShapes size = ([box],  baryCenterOffset)
  where
    box = Polygon [upperLeft, lowerLeft, lowerRight, upperRight]

    Size w h :: Size CpFloat = fmap realToFrac size
    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh

    low = hh
    up = - hh
    left = (- wh)
    right = wh

    upperLeft = Vector left up
    lowerLeft = Vector left low
    lowerRight = Vector right low
    upperRight = Vector right up
