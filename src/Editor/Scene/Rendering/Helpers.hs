
module Editor.Scene.Rendering.Helpers where


import Data.Abelian

import Graphics.Qt

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

-- | renders the given object (with the given Transformation)
renderEditorObject :: Sort sort o => Ptr QPainter -> Offset Double -> EditorObject sort -> IO ()
renderEditorObject ptr offset eo =
    sortRenderTransformed ptr offset eo Nothing

-- | renders a sort with the given transformations in the scene
sortRenderTransformed :: Sort sort o => Ptr QPainter -> Offset Double -> EditorObject sort
    -> Maybe (Size Double) -> IO ()
sortRenderTransformed ptr offset eo Nothing = do
    resetMatrix ptr
    let ep = editorPosition eo
        sort = editorSort eo
        pos = editorPosition2QtPosition sort ep
        offsetPlusPosition = offset +~ pos
    translate ptr offsetPlusPosition
    sortRender sort ptr (InScene offsetPlusPosition) (editorOEMState eo)

sortRenderTransformed ptr offset eo (Just boxSize) = do
    resetMatrix ptr
    translate ptr offset

    let ep = editorPosition eo
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

    sortRender sort ptr Iconified (editorOEMState eo)

