
module Base.Renderable.Centered where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Types
import Base.Renderable.Common ()


centered :: Renderable r => r -> RenderableInstance
centered = RenderableInstance . Centered . RenderableInstance

data Centered = Centered RenderableInstance

instance Renderable Centered where
    label = const "Centered"
    render ptr app config parentSize (Centered child) = return $ tuple parentSize $ do
        (childSize, action) <- render ptr app config parentSize child
        let offset = notLessThanZero $ size2position $
                fmap (fromIntegral . (round :: Double -> Int) . (/ 2)) (parentSize -~ childSize)
        translate ptr offset
        action

notLessThanZero (Position x y) = Position (max 0 x) (max 0 y)
