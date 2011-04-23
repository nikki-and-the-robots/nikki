
module Base.Renderable.Centered where


import Data.Abelian

import Graphics.Qt

import Base.Types
import Base.Renderable.Common ()


centered :: Renderable r => r -> RenderableInstance
centered = RenderableInstance . Centered . RenderableInstance

data Centered = Centered RenderableInstance
  deriving Show

instance Renderable Centered where
    minimalSize app (Centered x) = minimalSize app x
    render ptr app parentSize (Centered child) =
        translate ptr offset >>
        render ptr app childSize child
      where
        childSize = minimalSize app child
        offset = sizeToPosition $ fmap (fromIntegral . round . (/ 2)) (parentSize -~ childSize)
