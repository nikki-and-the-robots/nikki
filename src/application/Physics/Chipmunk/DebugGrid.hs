{-# language NamedFieldPuns #-}

module Physics.Chipmunk.DebugGrid (
    renderGrid,
  ) where


import Graphics.Qt as Qt

import Physics.Hipmunk

import Physics.Chipmunk.Types

import Utils


renderGrid :: Ptr QPainter -> Chipmunk -> IO ()
renderGrid ptr chip = do
    (position, rad) <- getChipmunkPosition chip

    translateVector ptr position
    Qt.rotate ptr (rad2deg rad)

    mapM_ (renderShapeType ptr) (shapeTypes chip)

renderShapeType :: Ptr QPainter -> ShapeType -> IO ()
renderShapeType ptr Polygon{vertices} =
    mapM_ (uncurry (renderVectorLine ptr)) (adjacentCyclic vertices)
renderShapeType ptr (LineSegment start end thickness) =
    renderVectorLine ptr start end
renderShapeType ptr st = nm "renderShape" st

renderVectorLine :: Ptr QPainter -> Vector -> Vector -> IO ()
renderVectorLine ptr (Vector x1 y1) (Vector x2 y2) = do
    setPenColor ptr 255 255 255 128
    drawLine ptr (Position x1 y1) (Position x2 y2)

