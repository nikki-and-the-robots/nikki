{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.Box where


import Data.Typeable
import Data.Abelian

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base.Constants

import Object


-- Configuration

names = ["box-wood-large", "box-wood-small"]

-- loading

sorts :: IO [Sort_]
sorts =
    mapM mkSort_ names
  where
    mkSortId name = SortId ("objects" </> name)
    mkSort_ name = do
        pix <- newQPixmap $ mkPath name
        size <- fmap fromIntegral <$> sizeQPixmap pix
        return $ Sort_ $ BSort (mkSortId name) pix size

mkPath :: String -> FilePath
mkPath name = pngDir </> "objects" </> name <.> "png"

data BSort
    = BSort {
        sortId_ :: SortId,
        pixmap :: (Ptr QPixmap),
        size_ :: Size Double
      }
  deriving (Show, Typeable)

data Box = Box {chipmunk_ :: Chipmunk}
    deriving (Show, Typeable)

instance Sort BSort Box where
    sortId = sortId_
    size = size_
    sortRender sort =
        sortRenderSinglePixmap (pixmap sort) sort
    initialize sort (Just space) editorPosition Nothing = do
        let (shapes, baryCenterOffset) = mkShapes $ size sort
            shapesWithAttributes = map (tuple shapeAttributes) shapes
            position = qtPositionToVector (editorPosition2QtPosition sort editorPosition)
                            +~ baryCenterOffset
        chip <- CM.initChipmunk space (bodyAttributes position (size sort)) shapesWithAttributes baryCenterOffset
        return $ Box chip
    chipmunk = chipmunk_
    update o _ _ _ = return o
    render o sort ptr offset now = do
        renderChipmunk ptr offset (pixmap sort) (chipmunk_ o)


bodyAttributes :: CM.Position -> Size QtReal -> BodyAttributes
bodyAttributes pos (Size a b) = BodyAttributes{
    CM.position            = pos,
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



