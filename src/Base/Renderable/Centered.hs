
module Base.Renderable.Centered where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Types
import Base.Renderable.Common


centered :: Renderable r => r -> RenderableInstance
centered = RenderableInstance . Centered . RenderableInstance

data Centered = Centered RenderableInstance
  deriving Show

instance Renderable Centered where
    render ptr app config parentSize (Centered child) = return $ tuple parentSize $ do
        (childSize, action) <- render ptr app config parentSize child
        let offset = sizeToPosition $ fmap (fromIntegral . round . (/ 2)) (parentSize -~ childSize)
        translate ptr $ uberRound offset
        action
