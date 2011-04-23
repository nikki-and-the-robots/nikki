
module Base.Renderable.Centered where


import Data.Abelian

import Graphics.Qt

import Base.Types
import Base.Renderable.Common ()


centered :: Renderable r => r -> RenderableInstance
centered = RenderableInstance . Centered . RenderableInstance

data Centered = Centered RenderableInstance

instance Renderable Centered where
    render app parentSize (Centered child) = (size, action)
      where
        (size, childAction) = render app parentSize child
        offset = sizeToPosition $ fmap (fromIntegral . round . (/ 2)) (parentSize -~ size)
        action ptr =
            translate ptr offset >>
            childAction ptr
