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
renderGrids ptr offset chips = do
    setPenColor ptr signalRed 1
    mapM_ (renderGrid ptr offset) chips

renderGrid :: Ptr QPainter -> Qt.Position Double -> Chipmunk -> IO ()
renderGrid ptr offset chip = do
    resetMatrix ptr
    translate ptr offset

    (position, rad) <- getChipmunkPosition chip

    translateVector ptr position
    Qt.rotate ptr (rad2deg rad)
    when (position /= zero) $
        drawCircle ptr zero 5

    mapM_ (renderShapeType ptr) (shapeTypes chip)

renderShapeType :: Ptr QPainter -> ShapeDescription -> IO ()
renderShapeType ptr ShapeDescription{shapeType, shapeOffset} =
    case (shapeType, shapeOffset) of
        (Polygon{vertices}, offset) ->
            mapM_ (uncurry (renderVectorLine ptr)) (adjacentCyclic $ map (offset +~) vertices)
        (LineSegment start end thickness, offset) ->
            renderVectorLine ptr (start +~ offset) (end +~ offset)
        (Circle radius, vec) ->
            drawCircle ptr (vector2position vec) (realToFrac radius)

renderVectorLine :: Ptr QPainter -> Vector -> Vector -> IO ()
renderVectorLine ptr a b =
    drawLine ptr (vector2position a) (vector2position b)
