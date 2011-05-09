
module Base.Options (
    generalOptions
  ) where


import Utils

import Base.Types
import Base.Configuration
import Base.Monad
import Base.Prose
import Base.Application

import Base.Renderable.Menu


-- | options that are used in the main menu and in the game (and possibly the editor?)
generalOptions :: Application -> Int -> Parent -> AppState
generalOptions app ps parent = NoGUIAppState $ do
    fullScreenMenuItem <- mkFullScreen app this
    let menuItems =
            fullScreenMenuItem :
            []
    return $ menuAppState app (Just $ p "options") (Just parent) menuItems ps
  where
    this ps = generalOptions app ps parent

mkFullScreen :: Application -> (Int -> AppState) -> M (Prose, Int -> AppState)
mkFullScreen app parent = do
    on <- gets fullscreen
    let switchText = if on then p "on" else p "off"
        text = p "fullscreen" +> pVerbatim ": " +> switchText
    return (text, \ ps -> NoGUIAppState (swapFullScreen app >> return (parent ps)))
