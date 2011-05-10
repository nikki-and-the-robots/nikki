
module Game.Menus where


import Utils

import Base.Types
import Base.Prose
import Base.Options
import Base.Monad
import Base.Paths
import Base.Application

import Base.Renderable.Common
import Base.Renderable.Menu
import Base.Renderable.Scrollable
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.StickToBottom
import Base.Renderable.Centered


pauseMenu :: Application -> AppState -> Int -> AppState
pauseMenu app follower = menuAppState app PauseMenu (Just follower) (
    (p "continue", const follower) :
--     (p "rewind to last save point", todo) :
--     (p "retry from beginning", todo) :
    (p "options", generalOptions app 0 . this) :
    (p "help", playHelp app . this) :
    (p "quit level", const FinalAppState) :
    [])
  where
    this = pauseMenu app follower


playHelp :: Application -> Parent -> AppState
playHelp app parent = NoGUIAppState $ do
    file <- rm2m $ getDataFileName "manual/playHelp.txt"
    text <- io $ pFile file
    return $ scrollingAppState app text parent

failureMenu :: Application -> AppState
failureMenu app = menuAppState app FailureMenu Nothing (
--     (p "rewind to last savepoint", todo) :
--     (p "retry from beginning", todo) :
    (p "quit level", const FinalAppState) :
    []) 0

-- | show a textual message and wait for a keypress
successMessage :: Application -> AppState
successMessage app = appState renderable $ do
    ignore $ waitForPressButton app
    return FinalAppState
  where
    renderable = MenuBackground |:>
        addKeysHint PressAnyKey (centered (successPixmap pixmaps))
    pixmaps = applicationPixmaps app
