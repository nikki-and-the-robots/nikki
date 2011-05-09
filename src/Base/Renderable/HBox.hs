
module Base.Renderable.HBox (hBox) where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Types

import Base.Renderable.Common


data HBox = HBox [RenderableInstance]

hBox :: Renderable r => [r] -> HBox
hBox = HBox . map RenderableInstance

instance Renderable HBox where
    label = const "HBox"
    render ptr app config size (HBox children_) = do
        children <- fmapM (render ptr app config size) children_
        let size = foldr addSizes zero $ fmap fst children
            action = forM_ children $ \ (childSize, childAction) -> do
                recoverMatrix ptr childAction
                translate ptr (Position (width childSize) 0)
        return (size, action)

-- | adds sizes horizontally
addSizes :: Size Double -> Size Double -> Size Double
addSizes (Size a b) (Size x y) =
    Size (a + x) (max b y)
