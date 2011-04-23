
module Base.Options (
    generalOptions
  ) where


import Base.Types
import Base.Monad

import Base.Renderable.Common
import Base.Renderable.Menu


-- | options that are used in the main menu and in the game (and possibly the editor?)
generalOptions :: Application_ s -> AppState -> AppState
generalOptions app parent = menu app "options" (Just parent)
   (("fullscreen", AppState (rt "generalOptions") (swapFullScreen app >> return this)) :
   [])
  where
    this = generalOptions app parent
