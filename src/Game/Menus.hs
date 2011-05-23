
module Game.Menus where


import Utils

import Base

import Base.Renderable.Common
import Base.Renderable.Menu
import Base.Renderable.Scrollable
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.StickToBottom
import Base.Renderable.Centered
import Base.Renderable.VBox
import Base.Renderable.Spacer


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
successMessage :: Application -> Score -> (Record, Record) -> AppState
successMessage app score (timeRecord, batteryRecord) = appState renderableInstance $ do
    ignore $ waitForPressButton app
    return FinalAppState
  where
    renderableInstance = MenuBackground |:>
        addKeysHint PressAnyKey (centered (vBox (length lines) lines))
    lines :: [RenderableInstance]
    lines =
        renderable (successPixmap (applicationPixmaps app)) :
        lineSpacer :
        renderable (mkScoreProse score) :
        fmap renderable (mkTimeRecord timeRecord) ++
        fmap renderable (mkBatteryRecord batteryRecord) ++
        []

mkTimeRecord, mkBatteryRecord :: Record -> [Prose]
mkTimeRecord NoNewRecord = []
mkTimeRecord NewRecord = p "new best time!" : []
mkTimeRecord RecordTied = p "time record tied!" : []
mkBatteryRecord NoNewRecord = []
mkBatteryRecord NewRecord = p "new battery record!" : []
mkBatteryRecord RecordTied = p "battery record tied!" : []
