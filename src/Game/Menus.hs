
module Game.Menus where


import Graphics.Qt

import Utils

import Base

import Base.Renderable.Layered
import Base.Renderable.StickToBottom
import Base.Renderable.Centered
import Base.Renderable.CenterHorizontally
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
successMessage :: Application -> Score -> (Maybe Score, Record, Record) -> AppState
successMessage app score (mHighScore, timeRecord, batteryRecord) = appState renderableInstance $ do
    controls__ <- controls_ <$> getConfiguration
    ignore $ waitForSpecialPressButton app (isMenuConfirmation controls__)
    return FinalAppState
  where
    renderableInstance = MenuBackground |:>
        addKeysHint (menuConfirmationKeysHint (Base.p "ok"))
            (centered $ vBox (length lines) $ fmap centerHorizontally lines)
    lines :: [RenderableInstance]
    lines =
        renderable (successPixmap (applicationPixmaps app)) :
        lineSpacer :
        renderable batteryLine :
        renderable timeLine :
        []
    batteryLine :: [Glyph]
    batteryLine =
        currentBattery ++ batteryRecordGlyphs
    currentBattery :: [Glyph]
    currentBattery =
        pvdWhite $
        (batteryChar : " " ++ batteryFormat (score ^. scoreBatteryPower))
    batteryRecordGlyphs :: [Glyph]
    batteryRecordGlyphs =
        bracket $
        case batteryRecord of
            NoNewRecord ->
                maybe (pv "no record") (\ hs ->
                    p "record" ++ pv ": " ++ pvd (batteryFormat hs))
                    (batteryHighScore mHighScore)
            NewRecord ->
                p "new record!" ++
                maybe (pv "") (\ hs ->
                    pv " " ++ p "before" ++ pv ": " ++ pvd (batteryFormat hs))
                    (batteryHighScore mHighScore)
            RecordTied -> p "tie record"
    batteryHighScore :: Maybe Score -> Maybe Integer
    batteryHighScore Nothing = Nothing
    batteryHighScore (Just hs) =
        case hs ^. scoreBatteryPower of
            0 -> Nothing
            n -> Just n

    timeLine :: [Glyph]
    timeLine =
        currentTime ++ timeRecordGlyphs
    currentTime =
        pvdWhite $
        (watchChar : " " ++ timeFormat (score ^. scoreTime))
    timeRecordGlyphs =
        bracket $
        case timeRecord of
            NoNewRecord ->
                maybe (pv "") (\ hs ->
                    p "record" ++ pv ": " ++ pvd (timeFormat (hs ^. scoreTime)))
                    mHighScore
            NewRecord ->
                p "new record!" ++
                maybe (pv "") (\ hs ->
                    pv " " ++ p "before" ++ pv ": " ++ pvd (timeFormat (hs ^. scoreTime)))
                    mHighScore
            RecordTied -> p "tie record"


    -- String to [Glyph]
    -- | normal font
    p :: String -> [Glyph]
    p = proseToGlyphs (standardFont app) . capitalizeProse . Base.p
    pv :: String -> [Glyph]
    pv = proseToGlyphs (standardFont app) . capitalizeProse . Base.pv
    -- | digits font
    pvd :: String -> [Glyph]
    pvd = proseToGlyphs (digitFont app) . capitalizeProse . Base.pv
    -- | white
    pvdWhite :: String -> [Glyph]
    pvdWhite = proseToGlyphs (digitFont app) .
        colorizeProse white . capitalizeProse .
        Base.pv
    -- | Encloses in standard font brackets and adds spaces as a prefix,
    -- if the string is not null.
    bracket :: [Glyph] -> [Glyph]
    bracket [] = []
    bracket x = pv "  [" ++ x ++ pv "]"
