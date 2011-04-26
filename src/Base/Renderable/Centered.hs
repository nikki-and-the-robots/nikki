
module Base.Renderable.Centered where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Types
import Base.Renderable.Common


centered :: Renderable r => r -> RenderableInstance
centered = RenderableInstance . Centered . RenderableInstance

data Centered = Centered RenderableInstance

instance Renderable Centered where
    render ptr app parentSize (Centered child) = tuple parentSize $ do
        translate ptr $ uberRound offset
        action
      where
        (childSize, action) = render ptr app parentSize child
        offset = sizeToPosition $ fmap (fromIntegral . round . (/ 2)) (parentSize -~ childSize)
