
module Base.Options (
    generalOptions
  ) where


import Base.Types
import Base.Monad
import Base.Application.Widgets.Menu


-- | options that are used in the main menu and in the game (and possibly the editor?)
generalOptions :: Application_ s -> AppState -> AppState
generalOptions app parent = menu app (Just "options") (Just parent)
   (("fullscreen", AppState (swapFullScreen app >> return this)) :
   [])
  where
    this = generalOptions app parent
