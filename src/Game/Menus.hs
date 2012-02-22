
module Game.Menus where


import Data.IORef

import Physics.Chipmunk (freeSpace)

import Graphics.Qt

import Utils

import Base

import Base.Renderable.Layered
import Base.Renderable.StickToBottom
import Base.Renderable.Centered
import Base.Renderable.CenterHorizontally
import Base.Renderable.VBox
import Base.Renderable.Spacer

import Game.Scene
import Game.BackgroundScene as BackgroundScene


freeGameState :: GameState -> AppState -> AppState
freeGameState gameState follower = NoGUIAppState $ io $ do
    postGUI $ fmapM_ freeObject (scene gameState ^. objects)
    freeSpace $ cmSpace gameState
    return follower


pauseMenu :: Application -> Parent -> AppState -> GameState -> Int -> AppState
pauseMenu app parent continueLevel gameState =
  menuAppState app PauseMenu (Just continueLevel) (
    (p "continue", const continueLevel) :
--     (p "rewind to last save point", todo) :
    (p "retry from beginning", const $ freeGameState gameState (retryLevel gameState)) :
    (p "options", generalOptions app 0 . this) :
    (p "help", playHelp app . this) :
    (p "quit level", const $ freeGameState gameState parent) :
    [])
  where
    this = pauseMenu app parent continueLevel gameState


playHelp :: Application -> Parent -> AppState
playHelp app parent = NoGUIAppState $ do
    file <- rm2m $ getDataFileName "manual/playHelp.txt"
    controls_ <- gets (^. controls)
    text <- (fmap (substitute (keysContext controls_))) <$> io (pFile file)
    return $ scrollingAppState app text parent

failureMenu :: Application -> Parent -> RenderStateRefs -> GameState
    -> AppState
failureMenu app parent sceneRenderState gameState =
  AppStateLooped (renderable waitFailureScreen) $ do
    gameStateRef <- io $ newIORef gameState
    ignore $ waitForPressedButtonBackgroundScene app gameStateRef
                 (sceneMVar sceneRenderState) (const False) (Just afterLevelWaitTime)
    return $ menuAppStateSpecialized app (poller gameStateRef)
        (renderable backGround) AppStateLooped
        FailureMenu Nothing menuItems 0
  where
    menuItems =
--     (p "rewind to last savepoint", todo) :
        (p "retry from beginning", const $ freeGameState gameState (retryLevel gameState)) :
        (p "quit level", const $ freeGameState gameState parent) :
        []

    poller gameStateRef =
        waitForPressedButtonBackgroundScene app gameStateRef (sceneMVar sceneRenderState)
            (const True) Nothing
    backGround =
        sceneRenderState |:>
        MenuBackgroundTransparent
    waitFailureScreen =
        backGround |:>
        (addBottomLineSpacer $ centered $ vBox (length lines) $ lines)
    lines =
        renderable (failurePixmap (applicationPixmaps app)) :
        replicate (length menuItems + 1) lineSpacer ++
        []

-- | show a textual message and wait for a keypress
successMessage :: Application -> Parent -> RenderStateRefs -> GameState
    -> Score -> (Maybe Score, Record, Record) -> AppState
successMessage app parent sceneRenderState gameState score
  (mHighScore, timeRecord, batteryRecord) =
     AppStateLooped (renderable $ renderableInstance False) $ do
        ref <- io $ newIORef gameState
        waitForEvent ref (const False) (Just afterLevelWaitTime)
        return $ AppStateLooped (renderable $ renderableInstance True) $ do
            waitForEvent ref (const True) Nothing
            return $ freeGameState gameState parent
  where
    waitForEvent ref p to = ignore $ waitForPressedButtonBackgroundScene app ref
                                     (sceneMVar sceneRenderState) p to
    renderableInstance showKeyHint =
        sceneRenderState |:>
        MenuBackgroundTransparent |:>
        (if showKeyHint
            then addKeysHint (menuConfirmationKeysHint (Base.p "ok"))
            else addBottomLineSpacer)
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
