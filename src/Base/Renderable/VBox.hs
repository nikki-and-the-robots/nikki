
module Base.Renderable.VBox (vBox) where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Types

import Base.Renderable.Common ()


data VBox = VBox Int [RenderableInstance]
  deriving Show

-- | Creates a VBox, that will at least display n items.
-- Displays as much as possible.
vBox :: Renderable r => Int -> [r] -> VBox
vBox n = VBox n . map RenderableInstance

instance Renderable VBox where
    render ptr app config parentSize vBox@(VBox minimalItems items) = do
        itemRenders <- inner minimalItems (height parentSize) items
        return (vBoxSize (fmap fst itemRenders), renderVBox itemRenders)
      where
        vBoxSize itemSizes = case itemSizes of
            [] -> zero
            _ -> Size (maximum (fmap width itemSizes)) (boxHeight itemSizes)
        boxHeight itemSizes =
            if length itemSizes < length items
            then height parentSize
            else sum (fmap height itemSizes)

        inner :: Int -> Double -> [RenderableInstance] -> IO [(Size Double, IO ())]
        inner minimalItems h (a : r) = do
            t@(itemSize, action) <- render ptr app config (Size (width parentSize) h) a
            if (h >= height itemSize) || minimalItems > 0 then do
                rest <- inner (pred minimalItems) (h - height itemSize) r
                return (t : rest)
              else
                return []
        inner _ _ [] = return []

        renderVBox = fmapM_ $ \ (itemSize, action) -> do
            recoverMatrix ptr action
            translate ptr (Position 0 (height itemSize))
