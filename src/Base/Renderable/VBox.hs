
module Base.Renderable.VBox where


import Graphics.Qt

import Utils

import Base.Types

import Base.Renderable.Common ()


data VBox = VBox [RenderableInstance]

vBox :: Renderable r => [r] -> VBox
vBox = VBox . map RenderableInstance

instance Renderable VBox where
    render app size (VBox items) = tuple size renderAction
      where
        rItems = fmap (render app size) items
        itemSizes = fmap fst rItems
        size = Size (maximum (fmap width itemSizes)) (sum (fmap height itemSizes))
        renderAction ptr =
            fmapM_ (inner ptr) rItems
        inner :: Ptr QPainter -> (Size Double, Ptr QPainter -> IO ()) -> IO ()
        inner ptr (size, action) = do
            recoverMatrix ptr $
                action ptr
            translate ptr (Position 0 (height size))
