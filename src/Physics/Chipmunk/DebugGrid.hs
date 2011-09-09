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
        (Polygon{vertices}, offset) ->
            mapM_ (uncurry (renderVectorLine ptr)) (adjacentCyclic $ map (offset +~) vertices)
        (LineSegment start end thickness, offset) ->
            renderVectorLine ptr (start +~ offset) (end +~ offset)
        (Circle radius, vec) -> do
            setPenColor ptr signalRed 1
            drawCircle ptr (vector2position vec) (realToFrac radius)
        st -> nm "renderShapeType" st

renderVectorLine :: Ptr QPainter -> Vector -> Vector -> IO ()
renderVectorLine ptr a b = do
    setPenColor ptr signalRed 1
    drawLine ptr (vector2position a) (vector2position b)
