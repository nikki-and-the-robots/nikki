
module Base.Renderable.StickToBottom (
    stickToBottom,
    addBottomLineSpacer,
    addKeysHint,
    KeysHint(..),
  ) where


import Data.Monoid
import Data.List

import Graphics.Qt

import Utils

import Base.Types
import Base.Prose

import Base.Configuration.Controls

import Base.Renderable.Common ()
import Base.Renderable.CenterHorizontally
import Base.Renderable.Prose ()
import Base.Renderable.Spacer


-- | Implements a Renderable that has one child that gets rendered as
-- far down as possible. The other widget gets the rest of the space.
stickToBottom :: (Renderable head, Renderable bottom) =>
    head -> bottom -> RenderableInstance
stickToBottom head bottom = renderable $ StickToBottom (renderable head) (renderable bottom)

addBottomLineSpacer :: Renderable r => r -> RenderableInstance
addBottomLineSpacer child = stickToBottom child lineSpacer


-- * keys hints

-- | adds a hint which keys are in use at the bottom.
addKeysHint :: Renderable r => KeysHint -> r -> RenderableInstance
addKeysHint keys mainChild = stickToBottom mainChild
    (centerHorizontally $ (False, formatKeys keys))

-- | Converts a list of key hints to a user readable string.
formatKeys :: KeysHint -> Prose
formatKeys (KeysHint list) =
    capitalizeProse $
    colorizeProse white $
    brackets inner
  where
    inner = mconcat $ intersperse separator $
            map formatFunction list
    formatFunction (function, keys) = function +> pVerbatim ": " +> keys

    separator = pVerbatim "]      ["
formatKeys PressAnyKey =
    capitalizeProse $
    colorizeProse white $
    brackets $
    p "press any key"


-- * renderable implementation

data StickToBottom = StickToBottom RenderableInstance RenderableInstance

instance Renderable StickToBottom where
    label = const "StickToBottom"
    render ptr app config size (StickToBottom head bottom) = do
        (bottomSize, bottomR) <- render ptr app config size bottom
        let headSize = Size (width size) (height size - height bottomSize)
        (_, headR) <- render ptr app config headSize head
        return $ tuple size $ do
            recoverMatrix ptr headR
            translate ptr $ Position 0 (height headSize)
            bottomR
