
module Base.Renderable.Layered (Layered, (|:>)) where


import Graphics.Qt

import Utils

import Base.Types
import Base.Renderable.Common ()


data Layered =
    Layered RenderableInstance RenderableInstance

(|:>) :: (Renderable a, Renderable b) => a -> b -> Layered
a |:> b = Layered (RenderableInstance a) (RenderableInstance b)

instance Renderable Layered where
    render ptr app parentSize (Layered a b) =
        (size, action)
      where
        size = Size (withView (width . fst) max aR bR) (withView (height . fst) max aR bR)
        action = recoverMatrix ptr (snd aR) >> snd bR
        aR = render ptr app parentSize a
        bR = render ptr app parentSize b
