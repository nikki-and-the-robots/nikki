
module Editor.Scene.Rendering.Helpers where


import Graphics.Qt

import Utils

import Base


-- draw a box at the given position with the given size
drawBox :: Ptr QPainter -> Position Double -> Size Double -> Double -> IO ()
drawBox ptr (Position x y) (Size w h) 0 = return ()
drawBox ptr (Position x y) (Size w h) thickness = do
    resetMatrix ptr
    drawRect ptr (Position (x - 1) (y - 1)) (Size (w + 1) (h + 1))
    drawBox ptr (Position (x - 1) (y - 1)) (Size (w + 2) (h + 2)) (thickness - 1)

-- | same as $drawBox$, but with color
drawColoredBox :: Ptr QPainter -> Position Double -> Size Double
    -> Double -> Color -> IO ()
drawColoredBox ptr position size thickness color = do
    setPenColor ptr color 1
    drawBox ptr position size thickness

-- | Renders a sort icon in the given size.
sortRenderIconified :: Sort sort o => Ptr QPainter -> Offset Double
    -> EditorObject sort -> Size Double -> IO ()
sortRenderIconified ptr offset eo boxSize = do
    resetMatrix ptr
    translate ptr offset

    let ep = eo ^. editorPosition
        pos = Position (editorX ep) (editorY ep - height boxSize)
    translate ptr pos

    let sort = editorSort eo
        factor = min (height boxSize / height (size sort))
                     (width boxSize / width (size sort))
        xScalingOffset = max 0 ((width boxSize - factor * width (size sort)) / 2)
        yScalingOffset = max 0 ((height boxSize - factor * height (size sort)) / 2)
        scalingOffset = Position xScalingOffset yScalingOffset

    translate ptr scalingOffset
    scale ptr factor factor

    renderIconified sort ptr

