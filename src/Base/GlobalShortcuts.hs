
module Base.GlobalShortcuts (handleGlobalShortcuts) where


import Control.Monad

import System.Exit

import Utils

import Base.Types
import Base.Monad
import Base.Configuration.Controls


handleGlobalShortcuts :: Application -> [AppEvent] -> M [AppEvent]
handleGlobalShortcuts app =
    filterM $ handler app

handler :: Application -> AppEvent -> M Bool
handler app x =
    case shortcuts app x of
        Nothing -> return True
        (Just h) -> h >> return False

shortcuts :: Application -> AppEvent -> Maybe (M ())
shortcuts app e = case e of
    Base.Types.CloseWindow -> Just $ io $ exitWith ExitSuccess
    (Press k) | fullscreenSwitch -> Just $ swapFullScreen app
      where
        fullscreenSwitch = isFullscreenSwapShortcut k
    _ -> Nothing
