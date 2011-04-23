
module Base.Renderable.Layered (Layered, (|:>)) where


import Graphics.Qt

import Utils

import Base.Types
import Base.Renderable.Common


data Layered =
    Layered RenderableInstance RenderableInstance

(|:>) :: (Renderable a, Renderable b) => a -> b -> Layered
a |:> b = Layered (RenderableInstance a) (RenderableInstance b)

instance Renderable Layered where
    render app parentSize (Layered a b) = (size, action)
      where
        (aSize, aAction) = render app parentSize a
        (bSize, bAction) = render app parentSize b
        size = Size (withView width max aSize bSize) (withView height max aSize bSize)
        action ptr = do
            recoverMatrix ptr $
                aAction ptr
            bAction ptr
