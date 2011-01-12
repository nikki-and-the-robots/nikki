
module Base.GlobalShortcuts (handleGlobalShortcuts) where


import Control.Monad

import System.Exit

import Graphics.Qt

import Utils

import Base.Types
import Base.Monad


handleGlobalShortcuts :: Application_ s -> [AppEvent] -> M [AppEvent]
handleGlobalShortcuts app =
    filterM (handler app)

handler :: Application_ s -> AppEvent -> M Bool
handler app x =
    case shortcuts app x of
        Nothing -> return True
        (Just h) -> h >> return False

shortcuts :: Application_ s -> AppEvent -> Maybe (M ())
shortcuts app x = case x of
    Quit -> Just $ io $ exitWith ExitSuccess
    _ -> Nothing
