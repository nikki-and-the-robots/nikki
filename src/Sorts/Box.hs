{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.Box (sorts) where


import Data.Typeable
import Data.Abelian

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Base

import Object


-- Configuration

names =
    ("box-wood-small", 125) :
    ("box-wood-large", 370) :
    []

-- loading

sorts :: RM [Sort_]
sorts =
    mapM (uncurry mkSort_) names
  where
    mkSortId name = SortId ("objects/" ++ name)
    mkSort_ name mass = do
        pngFile <- getDataFileName $ mkPath name
        pix <- loadPixmap (Position 1 1) pngFile
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
    initialize sort (Just space) editorPosition Nothing = do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            shapesWithAttributes = map (mkShapeDescription shapeAttributes) shapes
            position = position2vector (editorPosition2QtPosition sort editorPosition)
                            +~ baryCenterOffset
            bodyAttributes = mkBodyAttributes shapes position (boxMass sort)
        chip <- CM.initChipmunk space bodyAttributes
                    shapesWithAttributes baryCenterOffset
        return $ Box chip
    immutableCopy (Box x) = CM.immutableCopy x >>= return . Box
    chipmunks b = [chipmunk b]
    render o sort ptr offset now = do
        (pos, angle) <- getRenderPositionAndAngle (chipmunk o)
        return [RenderPixmap (boxPixmap sort) pos (Just angle)]

shapeAttributes :: ShapeAttributes
shapeAttributes = ShapeAttributes {
    elasticity    = 0.5,
    friction      = 0.95,
    collisionType = TileCT
  }

mkShapes :: Size Double -> ([ShapeType], Vector)
mkShapes (Size w h) = ([box],  baryCenterOffset)
  where
    box = Polygon [upperLeft, lowerLeft, lowerRight, upperRight]

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
