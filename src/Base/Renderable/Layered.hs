
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
    render ptr app config parentSize (Layered a b) = do
        aR <- render ptr app config parentSize a
        bR <- render ptr app config parentSize b
        return (size aR bR, action aR bR)
      where
        size aR bR = Size (withView (width . fst) max aR bR) (withView (height . fst) max aR bR)
        action aR bR = recoverMatrix ptr (snd aR) >> snd bR
