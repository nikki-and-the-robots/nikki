{-# language ScopedTypeVariables #-}

-- | Things used in multiple Widgets.

module Base.Application.Widgets.Common where


import Graphics.Qt

import Utils

import Base.Types
import Base.Polling


backgroundColor :: Color = QtColor 16 0 156 255
standardFontColor :: Color = QtColor 115 115 255 255

-- | Blocks until a Press AppEvent is received.
-- Flushes the event queue before that.
waitForPressAppEvent :: Application_ s -> M Button
waitForPressAppEvent app = do
    ignore $ pollAppEvents app
    inner
  where
    inner = do
        e <- waitForAppEvent app
        case e of
            (Press b) -> return b
            _ -> inner
