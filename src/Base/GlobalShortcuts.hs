
module Base.GlobalShortcuts (handleGlobalShortcuts) where


import Data.Set

import Control.Monad

import System.Exit

import Graphics.Qt

import Utils

import Base.Types
import Base.Monad


handleGlobalShortcuts :: Application_ s -> Set Button -> [AppEvent] -> M [AppEvent]
handleGlobalShortcuts app held =
    filterM (handler app held)

handler :: Application_ s -> Set Button -> AppEvent -> M Bool
handler app held x =
    case shortcuts app held x of
        Nothing -> return True
        (Just h) -> h >> return False

shortcuts :: Application_ s -> Set Button -> AppEvent -> Maybe (M ())
shortcuts app held e = case e of
    Base.Types.CloseWindow -> Just $ io $ exitWith ExitSuccess
    (Press k) | fullscreenSwitch -> Just $ swapFullScreen app
      where
        fullscreenSwitch =
            (isEnterOrReturn k && fany (isKey Alt) held) ||
            (isKey F11 k)
    _ -> Nothing
