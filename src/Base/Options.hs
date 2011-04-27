
module Base.Options (
    generalOptions
  ) where


import Base.Types
import Base.Monad
import Base.Prose

import Base.Renderable.Common
import Base.Renderable.Menu


-- | options that are used in the main menu and in the game (and possibly the editor?)
generalOptions :: Application_ s -> Int -> Parent -> AppState
generalOptions app ps parent = menuAppState app (p "options") (Just parent)
   ((p "fullscreen", \ ps -> NoGUIAppState (swapFullScreen app >> return (this ps))) :
   []) ps
  where
    this ps = generalOptions app ps parent
