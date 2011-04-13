{-# language ScopedTypeVariables #-}

-- | Things used in multiple Widgets.

module Base.Application.Widgets.Common where


import Graphics.Qt

import Utils

import Base.Types
import Base.Polling


backgroundColor :: Color = blue
standardFontColor :: Color = red

-- | Blocks until a Press AppEvent is received.
-- Flushes the event queue before that.
waitForPressAppEvent :: Application_ s -> M Button
waitForPressAppEvent app = do
    void $ pollAppEvents app
    inner
  where
    inner = do
        e <- waitForAppEvent app
        case e of
            (Press b) -> return b
            _ -> inner
