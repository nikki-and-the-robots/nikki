{-# language NamedFieldPuns #-}

module Physics.Chipmunk.DebugGrid (
    renderGrids,
  ) where


import Data.Abelian

import Graphics.Qt as Qt

import Physics.Hipmunk as CM

import Physics.Chipmunk.Types

import Utils


renderGrids :: Ptr QPainter -> Qt.Position Double -> [Chipmunk] -> IO ()
renderGrids ptr offset chips =
    mapM_ (renderGrid ptr offset) chips

renderGrid :: Ptr QPainter -> Qt.Position Double -> Chipmunk -> IO ()
renderGrid ptr offset chip = do
    resetMatrix ptr
    translate ptr offset

    (position, rad) <- getChipmunkPosition chip

    translateVector ptr position
    Qt.rotate ptr (rad2deg rad)

    mapM_ (renderShapeType ptr) (shapeTypes chip)

renderShapeType :: Ptr QPainter -> ShapeDescription -> IO ()
renderShapeType ptr ShapeDescription{shapeType, shapeOffset} =
    case (shapeType, shapeOffset) of
        (Polygon{vertices}, Vector 0 0) ->
            mapM_ (uncurry (renderVectorLine ptr)) (adjacentCyclic vertices)
        (LineSegment start end thickness, Vector 0 0) ->
            renderVectorLine ptr start end
        (Circle radius, Vector x y) -> do
            setPenColor ptr 255 55 55 255 1
            drawCircle ptr (Position x y) radius
        st -> nm "renderShape" st

renderVectorLine :: Ptr QPainter -> Vector -> Vector -> IO ()
renderVectorLine ptr (Vector x1 y1) (Vector x2 y2) = do
    setPenColor ptr 255 55 55 255 1
    drawLine ptr (Position x1 y1) (Position x2 y2)

