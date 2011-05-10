{-# language NamedFieldPuns, ScopedTypeVariables #-}

-- | The game is made up of different 'AppState's (see Top.Application), but basically
-- these are just IO-operations. They represent the top level menu, the loading stages and
-- playing or editing a level.
-- This module also contains setting up the Qt stuff
-- (application, window, event polling).
--
-- There are two threads:
-- 1. The logic thread. This will do the physics simulation as well as game logic
-- 2. The rendering thread (which is also the bound main thread). 
--    This will just render to the widget.
--    Most of the states will use 'setDrawingCallbackGLContext' to set the rendering function.
--    This is the way, to let the rendering thread do stuff.
--
-- Forking takes place once in 'Top.main' forking the logic thread.

module Top.Main where


import Data.List as List
import Data.SelectTree (leafs)
import Data.Accessor.Monad.MTL.State ((%=))

import Text.Logging

import Control.Concurrent
import Control.Monad.CatchIO

import System.FilePath
import System.Exit

import Graphics.Qt

import Version

import Utils

import Distribution.AutoUpdate

import Base

import Editor.Scene (initEditorScene)
import Editor.Pickle
import Editor.Menu (editLevel)

import Top.Initialisation
import Top.Game (playLevel)


main :: [Key] -> IO ()
main initialSignals =
  withStaticConfiguration $ do
    logInfo ("Nikki and the Robots (" ++ showVersion nikkiVersion ++ ")")

    configuration <- ask

    -- qt initialisation
    withQApplication $ \ qApp -> do
        let Windowed windowSize = programWindowSize
        withGLContext 0 (width windowSize) (height windowSize) $ \ window -> do
            withNikkiIcon window $ do
                keyPoller <- io $ newKeyPoller window (initial_events configuration ++ initialSignals)

                -- showing main window
                let windowMode = if fullscreen configuration then FullScreen else programWindowSize
                io $ setWindowTitle window "Nikki and the Robots"
                io $ setWindowSize window windowMode
                io $ showGLContext window

                -- sort loading (pixmaps and sounds)
                withAllSorts $ \ sorts -> withApplicationPixmaps $ \ appPixmaps -> do

                    -- start state logick
                    let app :: Application
                        app = Application qApp window keyPoller (flip mainMenu 0) appPixmaps sorts
                        -- there are two main threads:
                        -- this is the logick [sick!] thread
                        -- dynamic changes of the configuration take place in this thread!
                        logicThread = do
                            withDynamicConfiguration configuration $
                                runAppState app (applicationStates app)
                    exitCodeMVar <- forkLogicThread $ do
                        logicThread `finally` quitQApplication

                    -- this is the rendering thread (will be quit by the logick thread)
                    exitCodeFromQApplication <- execQApplication qApp

                    exitCodeFromLogicThread <- takeMVar exitCodeMVar

                    case exitCodeFromLogicThread of
                        ExitFailure x -> exitWith $ ExitFailure x
                        ExitSuccess -> case exitCodeFromQApplication of
                            0 -> return ()
                            x -> exitWith $ ExitFailure x

withNikkiIcon :: Ptr GLContext -> RM a -> RM a
withNikkiIcon qWidget action = do
    iconPaths <- filter (("icon" `isPrefixOf`) . takeFileName) <$>
        getDataFiles pngDir (Just ".png")
    withApplicationIcon qWidget iconPaths action


-- * states

-- | top level application state
applicationStates :: Application -> AppState
applicationStates app = NoGUIAppState $ do
    mLevel <- gets play_level
    play_levelA %= Nothing
    case mLevel of
        Nothing -> return $ mainMenu app 0
        Just file -> return $ play app (mainMenu app 0) file

mainMenu :: Application -> Int -> AppState
mainMenu app ps =
    menuAppState app MainMenu Nothing (
        (p "story mode", storyMode app . this) :
        (p "community levels", community app 0 . this) :
        (p "options", generalOptions app 0 . this) :
        (p "help", mainMenuHelp app . this) :
        (p "online update", autoUpdate app . this) :
        (p "credits", credits app . this) :
        (p "quit", const $ FinalAppState) :
        []) ps
  where
    this = mainMenu app

-- | shows a text describing our plans with the story mode
storyMode :: Application -> Parent -> AppState
storyMode app parent = NoGUIAppState $ do
    file <- rm2m $ getDataFileName "manual/storyModeIntroduction"
    prose <- io $ pFile file
    return $ scrollingAppState app prose parent

credits :: Application -> Parent -> AppState
credits app parent = NoGUIAppState $ do
    file <- rm2m $ getDataFileName "manual/credits"
    prose <- io $ pFile file
    return $ scrollingAppState app prose parent

community :: Application -> Int -> Parent -> AppState
community app ps parent =
    menuAppState app (NormalMenu $ p "community levels") (Just parent) (
        (p "play levels", selectLevelPlay app . this) :
        (p "download levels", downloadLevels app . this) :
        (p "editor", selectLevelEdit app 0 . this) :
        []) ps
  where
    this ps = community app ps parent


downloadLevels :: Application -> Parent -> AppState
downloadLevels app parent = NoGUIAppState $ do
    file <- rm2m $ getDataFileName "manual/downloadLevels"
    prose <- io $ pFile file
    return $ scrollingAppState app prose parent


-- | asks, if the user really wants to quit
quit :: Application -> AppState -> Int -> AppState
quit app parent =
    menuAppState app (NormalMenu $ p "quit?") (Just parent) (
        (p "no", const $ parent) :
        (p "yes", const $ FinalAppState) :
        [])

-- | select a saved level.
selectLevelPlay :: Application -> Parent -> AppState
selectLevelPlay app parent = NoGUIAppState $ rm2m $ do
    levelFiles <- lookupPlayableLevels "choose a level"
    return $ if null $ leafs levelFiles then
        message app [p "no levels found :("] parent
      else
        treeToMenu app parent levelFiles (play app) 0

selectLevelEdit :: Application -> Int -> Parent -> AppState
selectLevelEdit app ps parent = menuAppState app (NormalMenu $ p "edit or create?") (Just parent) (
    (p "new level", pickNewLevelEdit app . this) :
    (p "edit existing level", selectExistingLevelEdit app . this) :
    []) ps
  where
    this ps = selectLevelEdit app ps parent

pickNewLevelEdit :: Application -> AppState -> AppState
pickNewLevelEdit app parent = NoGUIAppState $ rm2m $ do
    pathToEmptyLevel <- getDataFileName (templateLevelsDir </> "empty.nl")
    templateLevelPaths <- filter (not . ("empty.nl" `List.isSuffixOf`)) <$>
                          getDataFiles templateLevelsDir (Just ".nl")
    return $ menuAppState app (NormalMenu $ p "choose a template") (Just parent) (
        map mkMenuItem templateLevelPaths ++
        (p "empty level", const $ edit app parent (pathToEmptyLevel, True)) :
        []) 0
  where
    mkMenuItem templatePath =
        (pVerbatim $ takeBaseName templatePath, const $ edit app parent (templatePath, True))

selectExistingLevelEdit app parent = NoGUIAppState $ io $ do
    editableLevels <- lookupEditableLevels "choose a level"
    return $ if null $ leafs editableLevels then
        message app [p "no levels found :("] parent
      else
        treeToMenu app parent editableLevels (\ parent chosen -> edit app parent (chosen, False)) 0


play :: Application -> Parent -> FilePath -> AppState
play app parent file = loadingEditorScene app (file, False) (playLevel app parent)

edit :: Application -> Parent -> (FilePath, Bool) -> AppState
edit app parent file = loadingEditorScene app file (editLevel app)

-- | load a level, got to playing state afterwards
-- This AppState involves is a hack to do things from the logic thread 
-- in the rendering thread. Cause Qt's pixmap loading is not threadsafe.
loadingEditorScene :: Application -> (FilePath, Bool) -> (EditorScene Sort_ -> AppState) -> AppState
loadingEditorScene app (file, isTemplateFile) follower =
    appState (busyMessage $ p "loading...") $ io $ do
        grounds <- loadByFilePath (leafs $ allSorts app) file
        let mFile = if isTemplateFile then Nothing else Just file
        editorScene <- initEditorScene (allSorts app) mFile grounds
        return $ follower editorScene

mainMenuHelp :: Application -> Parent -> AppState
mainMenuHelp app parent = NoGUIAppState $ do
    file <- rm2m $ getDataFileName "manual/mainMenuHelp.txt"
    text <- io $ pFile file
    return $ scrollingAppState app text parent
