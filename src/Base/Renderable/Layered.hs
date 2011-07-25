
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
    label = const "Layered"
    render ptr app config parentSize (Layered a b) = do
        aR <- render ptr app config parentSize a
        bR <- render ptr app config parentSize b
        return (size aR bR, action aR bR)
      where
        size aR bR = Size (on max (width . fst) aR bR) (on max (height . fst) aR bR)
        action aR bR = recoverMatrix ptr (snd aR) >> snd bR
