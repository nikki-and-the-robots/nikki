
module Base.Renderable.CenterHorizontally where


import Graphics.Qt

import Base.Types
import Base.Renderable.Common


centerHorizontally :: Renderable r => r -> RenderableInstance
centerHorizontally = RenderableInstance . CenterHorizontally . RenderableInstance

data CenterHorizontally = CenterHorizontally RenderableInstance
  deriving Show

instance Renderable CenterHorizontally where
    render ptr app config parentSize (CenterHorizontally child) = do
        (childSize, childAction) <- render ptr app config parentSize child
        let size = Size (width parentSize) (height childSize)
            offset = (width parentSize - width childSize) / 2
            action = do
                translate ptr $ Position offset 0
                childAction
        return (size, action)
