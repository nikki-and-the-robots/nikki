{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.Box where


import Data.Typeable
import Data.Abelian

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Paths_nikki

import Base.Constants
import Base.Pixmap

import Object


-- Configuration

names = ["box-wood-small", "box-wood-large"]

-- loading

sorts :: IO [Sort_]
sorts =
    mapM mkSort_ names
  where
    mkSortId name = SortId ("objects/" ++ name)
    mkSort_ name = do
        pngFile <- getDataFileName $ mkPath name
        pix <- loadPixmap 1 pngFile
        return $ Sort_ $ BSort (mkSortId name) pix

mkPath :: String -> FilePath
mkPath name = pngDir </> "objects" </> name <.> "png"

data BSort
    = BSort {
        sortId_ :: SortId,
        boxPixmap :: Pixmap
      }
  deriving (Show, Typeable)

data Box = Box {chipmunk :: Chipmunk}
    deriving (Show, Typeable)

instance Sort BSort Box where
    sortId = sortId_
    size = pixmapSize . boxPixmap
    sortRender sort ptr _ =
        renderPixmapSimple ptr (boxPixmap sort)
    initialize sort (Just space) editorPosition Nothing = do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            shapesWithAttributes = map (mkShapeDescription shapeAttributes) shapes
            position = qtPosition2Vector (editorPosition2QtPosition sort editorPosition)
                            +~ baryCenterOffset
        chip <- CM.initChipmunk space (bodyAttributes position (size sort)) 
                    shapesWithAttributes baryCenterOffset
        return $ Box chip
    immutableCopy (Box x) = CM.immutableCopy x >>= return . Box
    chipmunks b = [chipmunk b]
    render o sort ptr offset now =
        renderChipmunk ptr offset (boxPixmap sort) (chipmunk o)


bodyAttributes :: CM.Position -> Size QtReal -> BodyAttributes
bodyAttributes pos (Size a b) = BodyAttributes {
    CM.position         = pos,
    mass                = (1 * (toKachel a) * (toKachel b)),
    inertia             = 6000
  }

shapeAttributes :: ShapeAttributes
shapeAttributes = ShapeAttributes {
    elasticity    = 0.5,
    friction      = 2,
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



