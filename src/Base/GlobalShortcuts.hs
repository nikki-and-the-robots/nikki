
module Base.GlobalShortcuts (handleGlobalShortcuts) where


import Control.Monad

import System.Exit

import Graphics.Qt

import Utils

import Base.Types
import Base.Monad


handleGlobalShortcuts :: [AppEvent] -> M [AppEvent]
handleGlobalShortcuts =
    filterM handler

handler :: AppEvent -> M Bool
handler x =
    case shortcuts x of
        Nothing -> return True
        (Just h) -> h >> return False

shortcuts :: AppEvent -> Maybe (M ())
shortcuts x = case x of
    Quit -> Just $ io $ exitWith ExitSuccess
    _ -> Nothing
