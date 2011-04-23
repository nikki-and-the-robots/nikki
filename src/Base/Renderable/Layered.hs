
module Base.Renderable.Layered (Layered, (|:>)) where


import Graphics.Qt

import Utils

import Base.Types
import Base.Renderable.Common ()


data Layered =
    Layered RenderableInstance RenderableInstance
  deriving Show

(|:>) :: (Renderable a, Renderable b) => a -> b -> Layered
a |:> b = Layered (RenderableInstance a) (RenderableInstance b)

instance Renderable Layered where
    minimalSize app (Layered a b) =
        Size (withView width max aSize bSize) (withView height max aSize bSize)
      where
        aSize = minimalSize app a
        bSize = minimalSize app b
    render ptr app parentSize (Layered a b) = do
        recoverMatrix ptr $
            render ptr app parentSize a
        render ptr app parentSize b
