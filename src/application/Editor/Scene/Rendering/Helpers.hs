
module Editor.Scene.Rendering.Helpers where


import Utils

import Data.Color
import Data.Abelian

import Graphics.Qt

import Base.Constants
import Base.Types

import Object


-- draw a box at the given position with the given size
drawBox :: Ptr QPainter -> Position Double -> Size Double -> Double -> IO ()
drawBox ptr (Position x y) (Size w h) 0 = return ()
drawBox ptr (Position x y) (Size w h) thickness = do
    resetMatrix ptr
    drawRect ptr (Position (x - 1) (y - 1)) (Size (w + 1) (h + 1))
    drawBox ptr (Position (x - 1) (y - 1)) (Size (w + 2) (h + 2)) (thickness - 1)

-- | same as $drawBox$, but with color
drawColoredBox :: Ptr QPainter -> Position Double -> Size Double -> Double -> RGBA -> IO ()
drawColoredBox ptr position size thickness (RGBA r g b a) = do
    setPenColor ptr (tb r) (tb g) (tb b) 127 1
    drawBox ptr position size thickness
  where
    tb :: Double -> QtInt
    tb x | x < 0 || x > 1 = es "tb in drawCursorBox" x
    tb x = truncate (x * 255)

-- | renders the given object (with the given Transformation)
renderEditorObject :: Ptr QPainter -> Offset Double -> EditorObject Sort_ -> IO ()
renderEditorObject ptr offset eo =
    sortRenderTransformed (editorSort eo) ptr offset (editorPosition eo) Nothing

-- | renders a sort with the given transformations in the scene
sortRenderTransformed :: Sort s o => s -> Ptr QPainter -> Offset Double -> EditorPosition
    -> Maybe (Size Double) -> IO ()
sortRenderTransformed sort ptr offset ep Nothing = do
    resetMatrix ptr
    let pos = editorPosition2QtPosition sort ep
        offsetPlusPosition = offset +~ pos
    translate ptr offsetPlusPosition
    sortRender sort ptr (InScene offsetPlusPosition)

sortRenderTransformed sort ptr offset ep (Just boxSize) = do
    resetMatrix ptr
    translate ptr offset

    let pos = Position (editorX ep) (editorY ep - height boxSize)
    translate ptr pos

    let factor = min (height boxSize / height (size sort))
                     (width boxSize / width (size sort))
        xScalingOffset = max 0 ((width boxSize - factor * width (size sort)) / 2)
        yScalingOffset = max 0 ((height boxSize - factor * height (size sort)) / 2)
        scalingOffset = Position xScalingOffset yScalingOffset

    translate ptr scalingOffset
    scale ptr factor factor

    sortRender sort ptr Iconified

