
module Base.Renderable.VBox where


import Graphics.Qt

import Utils

import Base.Types

import Base.Renderable.Common ()


data VBox = VBox [RenderableInstance]
  deriving Show

vBox :: Renderable r => [r] -> VBox
vBox = VBox . map RenderableInstance

instance Renderable VBox where
    minimalSize app (VBox items) =
        Size (maximum (fmap width itemSizes)) (sum (fmap height itemSizes))
      where
        itemSizes = fmap (minimalSize app) items
    render ptr app size vBox@(VBox items) =
        fmapM_ (inner $ width $ minimalSize app vBox) items
      where
        inner width item = do
            recoverMatrix ptr $
                translate ptr (Position 0 (- 20))
            let itemSize = minimalSize app item
            recoverMatrix ptr $
                render ptr app (Size width (height itemSize)) item
            translate ptr (Position 0 (height itemSize))
