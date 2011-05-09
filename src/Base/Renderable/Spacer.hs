{-# language FlexibleInstances #-}

module Base.Renderable.Spacer (emptySpacer, parentSpacer, lineSpacer) where


import Graphics.Qt

import Utils

import Base.Types
import Base.Font

import Base.Renderable.Common


-- | Widget with a size given by a function
emptySpacer :: (Size Double -> Size Double) -> RenderableInstance
emptySpacer fun = renderable $ Spacer fun Nothing

-- | Spacer that also has a child Renderable
parentSpacer :: Renderable r => (Size Double -> Size Double) -> r -> RenderableInstance
parentSpacer fun = renderable . Spacer fun . Just . renderable

-- | A spacer for on line of text
lineSpacer :: RenderableInstance
lineSpacer = renderable $ emptySpacer $ const $ Size 0 fontHeight

data Spacer = Spacer (Size Double -> Size Double) (Maybe RenderableInstance)

instance Renderable Spacer where
    label = const "Spacer"
    render ptr app config size (Spacer sizeFun mChild) = do
        let spacerSize = sizeFun size
        return $ tuple spacerSize $
            whenMaybe mChild $ (\ child -> snd =<< render ptr app config spacerSize child)
