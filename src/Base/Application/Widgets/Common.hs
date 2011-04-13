
-- | Things used in multiple Widgets.

module Base.Application.Widgets.Common where


import Utils

import Base.Types
import Base.Polling


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
