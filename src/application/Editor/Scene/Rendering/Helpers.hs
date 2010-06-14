
module Editor.Scene.Rendering.Helpers where


import Utils

import Data.Color
import Data.Abelian

import Graphics.Qt

-- import Base.Sprited

import Object.Types

-- import Editor.Scene.Types


-- | clears the whole screen
clearScreen :: Ptr QPainter -> IO ()
clearScreen ptr = do
    resetMatrix ptr
    windowSize <- sizeQPainter ptr
    eraseRect ptr zero windowSize (QtColor 0 0 0 255)


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
    setPenColor ptr (tb r) (tb g) (tb b) 127
    drawBox ptr position size thickness
  where
    tb :: Double -> QtInt
    tb x | x < 0 || x > 1 = es "tb in drawCursorBox" x
    tb x = truncate (x * 255)

-- | renders the given object (with the given Transformation)
renderEditorObject :: Ptr QPainter -> Offset -> EditorObject -> IO ()
renderEditorObject ptr offset eo = do
--     let sprited = eObjectSprited o
--         pos = eObjectPosition o
--         pix = defaultPixmap sprited
--     renderAvailableObject ptr (offset +~ pos) sprited
    sortRender (editorSort eo) ptr offset (editorPosition eo) Nothing





