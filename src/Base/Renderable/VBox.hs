
module Base.Renderable.VBox where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Types

import Base.Renderable.Common ()


data VBox = VBox [RenderableInstance]

vBox :: Renderable r => [r] -> VBox
vBox = VBox . map RenderableInstance

instance Renderable VBox where
    render ptr app parentSize vBox@(VBox items) =
        (vBoxSize, renderVBox)
      where
        vBoxSize = case itemSizes of
            [] -> zero
            _ -> Size (maximum (fmap width itemSizes)) (sum (fmap height itemSizes))
        itemSizes = map fst itemRenders
        itemRenders = inner (height parentSize) items

        inner :: Double -> [RenderableInstance] -> [(Size Double, IO ())]
        inner h (a : r) =
            let t@(itemSize, action) = render ptr app (Size (width parentSize) h) a
            in if (h >= height itemSize) then
                t : inner (h - height itemSize) r
              else []
        inner h [] = []

        renderVBox = forM_ itemRenders $ \ (itemSize, action) -> do
            recoverMatrix ptr action
            translate ptr (Position 0 (height itemSize))
