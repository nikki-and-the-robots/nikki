
module Base.Renderable.VBox where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Types

import Base.Renderable.Common ()


data VBox = VBox [RenderableInstance]
  deriving Show

vBox :: Renderable r => [r] -> VBox
vBox = VBox . map RenderableInstance

instance Renderable VBox where
    render ptr app config parentSize vBox@(VBox items) = do
        itemRenders <- inner (height parentSize) items
        return (vBoxSize (fmap fst itemRenders), renderVBox itemRenders)
      where
        vBoxSize itemSizes = case itemSizes of
            [] -> zero
            _ -> Size (maximum (fmap width itemSizes)) (sum (fmap height itemSizes))

        inner :: Double -> [RenderableInstance] -> IO [(Size Double, IO ())]
        inner h (a : r) = do
            t@(itemSize, action) <- render ptr app config (Size (width parentSize) h) a
            if (h >= height itemSize) then do
                rest <- inner (h - height itemSize) r
                return (t : rest)
              else
                return []
        inner h [] = return []

        renderVBox = fmapM_ $ \ (itemSize, action) -> do
            recoverMatrix ptr action
            translate ptr (Position 0 (height itemSize))
