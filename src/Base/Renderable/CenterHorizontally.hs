
module Base.Renderable.CenterHorizontally where


import Graphics.Qt

import Base.Types
import Base.Renderable.Common


centerHorizontally :: Renderable r => r -> RenderableInstance
centerHorizontally = RenderableInstance . CenterHorizontally . RenderableInstance

data CenterHorizontally = CenterHorizontally RenderableInstance

instance Renderable CenterHorizontally where
    render ptr app parentSize (CenterHorizontally child) =
        (size, action)
      where
        size = Size (width parentSize) (height childSize)
        (childSize, childAction) = render ptr app parentSize child
        action = do
            translate ptr $ uberRound (Position offset 0)
            childAction
        offset = (width parentSize - width childSize) / 2
