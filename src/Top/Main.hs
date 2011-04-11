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
import Control.Exception

import System.FilePath
import System.Exit

import Graphics.Qt

import Version

import Utils

import Distribution.AutoUpdate

import Base

import Object

import Editor.Scene (initEditorScene)
import Editor.Pickle

import Top.Initialisation
import Top.Editor (editLevel)
import Top.Game (playLevel)


main :: IO ()
main =
  withStaticConfiguration $ do
    logInfo ("Nikki and the Robots (" ++ showVersion nikkiVersion ++ ")")

    configuration <- ask

    -- qt initialisation
    qApp <- io newQApplication
    let Windowed windowSize = programWindowSize
    window <- io $ newGLContext 0 (width windowSize) (height windowSize)
    loadApplicationIcon window
    keyPoller <- io $ newKeyPoller window

    -- showing main window
    let windowMode = if fullscreen configuration then FullScreen else programWindowSize
    io $ setWindowTitle window "Nikki and the Robots"
    io $ setWindowSize window windowMode
    io $ showGLContext window

    -- sort loading (pixmaps and sounds)
    withAllSorts $ \ sorts -> withApplicationPixmaps $ \ appPixmaps -> do

        -- start state logick
        let app :: Application
            app = Application qApp window keyPoller mainMenu appPixmaps sorts
            -- there are two main threads:
            -- this is the logick [sick!] thread
            -- dynamic changes of the configuration take place in this thread!
            logicThread = do
                guiLog app (p "loading...")
                withDynamicConfiguration configuration $
                    executeStates (applicationStates app)
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

loadApplicationIcon :: Ptr GLContext -> RM ()
loadApplicationIcon qWidget = do
    iconPaths <- filter (("icon" `isPrefixOf`) . takeFileName) <$>
        getDataFiles pngDir (Just ".png")
    io $ setApplicationIcon qWidget iconPaths


-- * states

-- | top level application state
applicationStates :: Application -> AppState
applicationStates app = AppState $ do
    mLevel <- gets play_level
    play_levelA %= Nothing
    case mLevel of
        Nothing -> return $ mainMenu app
        Just file -> return $ play app (mainMenu app) file

mainMenu :: Application -> AppState
mainMenu app =
    menu app (Just title) Nothing [
        ("story mode", storyMode app),
        ("play", selectLevelPlay app this),
        ("help", mainMenuHelp app this),
        ("edit", selectLevelEdit app this),
        ("update", autoUpdate app this),
        ("quit", FinalState)
      ]
  where
    this = applicationStates app
    title = unlines (("NIKKI AND THE ROBOTS (" ++ showVersion nikkiVersion ++ ")") :
                     "(Use arrow keys and Ctrl key to select menu item.)" :
                     [])

-- | shows a text describing our plans with the story mode
storyMode :: Application -> AppState
storyMode app = AppState $ do
    file <- rm2m $ getDataFileName "manual/storyModeIntroduction"
    prose <- io $ pFile file
    return $ showText app prose (applicationStates app)

-- | asks, if the user really wants to quit
quit :: Application -> AppState -> AppState
quit app parent =
    menu app (Just "quit?") (Just parent) [
        ("no", applicationStates app),
        ("yes", FinalState)
      ]

-- | select a saved level.
selectLevelPlay :: Application -> AppState -> AppState
selectLevelPlay app parent = staticConfigAppState $ do
    levelFiles <- lookupLevels
    if null levelFiles then
        return $ menu app (Just "no levels found.") (Just parent) [("back", parent)]
      else do
            -- menu with the given selected item.
        let this preChoice = menuWithPreChoice
                app (Just "pick a level to play") (Just parent)
                (map (\ path -> (takeBaseName path, \ n -> play app (this n) path))
                    levelFiles)
                preChoice
        return $ this 0

selectLevelEdit :: Application -> AppState -> AppState
selectLevelEdit app parent = staticConfigAppState $ do
    freeLevelsPath <- getFreeLevelsDirectory
    levelFiles <- map (freeLevelsPath </>) <$> io (getFiles freeLevelsPath (Just "nl"))
    return $ menu app (Just "pick a level to edit") (Just parent) $
        ("new level", pickNewLevel app parent) :
        map (\ path -> (takeBaseName path, edit app parent (path, False))) levelFiles

pickNewLevel :: Application -> AppState -> AppState
pickNewLevel app parent = staticConfigAppState $ do
    pathToEmptyLevel <- getDataFileName (templateLevelsDir </> "empty.nl")
    templateLevelPaths <- filter (not . ("empty.nl" `List.isSuffixOf`)) <$>
                          getDataFiles templateLevelsDir (Just ".nl")
    return $ menu app (Just "pick a template to start from") (Just parent) $
        map mkMenuItem templateLevelPaths ++
        ("empty level", edit app parent (pathToEmptyLevel, True)) :
        []
  where
    mkMenuItem templatePath = (takeBaseName templatePath, edit app parent (templatePath, True))


play :: Application -> AppState -> FilePath -> AppState
play app parent file = loadingEditorScene app (file, False) (playLevel app parent)

edit :: Application -> AppState -> (FilePath, Bool) -> AppState
edit app parent file = loadingEditorScene app file (editLevel app playLevel)

-- | load a level, got to playing state afterwards
-- This AppState involves is a hack to do things from the logic thread 
-- in the rendering thread. Cause Qt's pixmap loading is not threadsafe.
loadingEditorScene :: Application -> (FilePath, Bool) -> (EditorScene Sort_ -> AppState) -> AppState
loadingEditorScene app (file, isTemplateFile) follower = ioAppState $ do
    guiLog app (p "loading...")
    grounds <- loadByFilePath (leafs $ allSorts app) file
    let mFile = if isTemplateFile then Nothing else Just file
    editorScene <- initEditorScene (allSorts app) mFile grounds
    return $ follower editorScene

mainMenuHelp :: Application -> AppState -> AppState
mainMenuHelp app parent = AppState $ do
    file <- rm2m $ getDataFileName "manual/mainMenuHelp.txt"
    text <- io $ pFile file
    return $ showText app text parent
