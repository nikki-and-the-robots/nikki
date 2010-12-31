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
--    Most of the states will use 'setDrawingCallbackAppWidget' to set the rendering function.
--    This is the way, to let the rendering thread do stuff.
--
-- Forking takes place once in 'Top.main' forking the logic thread.

module Top.Main where


import Data.List as List

import Control.Concurrent
import Control.Monad.Trans.Reader
import Control.Exception

import System.FilePath
import System.IO
import System.Exit
import System.Directory

import GHC.Conc

import Graphics.Qt

import Utils

import Base

import Object

import Editor.Scene (initEditorScene)
import Editor.Pickle

import Top.Initialisation
import Top.Editor (editLevel)
import Top.Game (playLevel)

main :: IO ()
main = globalCatcher $ withStaticConfiguration $ do

    io $ hSetBuffering stdout NoBuffering

    configuration <- ask

    -- qt initialisation
    qApp <- io newQApplication
    window <- io $ newAppWidget 0
    loadApplicationIcon window
    keyPoller <- io $ newKeyPoller window
    -- showing main window
    let windowSize = if fullscreen configuration then FullScreen else programWindowSize
    io $ setWindowSize window windowSize
    io $ showAppWidget window

    -- sort loading (pixmaps and sounds)
    withAllSorts $ \ sorts -> withApplicationPixmaps $ \ appPixmaps -> do

        -- start state logick
        let app = Application qApp window keyPoller applicationStates appPixmaps sorts
        -- there are two main threads:
        -- this is the logick [sick!] thread
        -- dynamic changes of the configuration take place in this thread!
        forkOS $ globalCatcher $ do
            (withDynamicConfiguration configuration $ executeStates (applicationStates app))
                `finally`
                quitQApplication

        -- this is the rendering thread (will be quit by the logick thread)
        code <- execQApplication qApp

        case code of
            0 -> exitWith ExitSuccess
            x -> exitWith (ExitFailure x)

loadApplicationIcon qApp = do
    iconPaths <- filter (("icon" `isPrefixOf`) . takeFileName) <$> getDataFiles ".png" pngDir
    io $ setApplicationIcon qApp iconPaths


-- * states

-- | top level application state (main menu)
applicationStates :: Application -> AppState
applicationStates app =
    menu app Nothing Nothing [
        ("story mode", storyMode app),
        ("play", selectLevelPlay app this),
        ("edit", selectLevelEdit app this),
        ("quit", FinalState)
      ]
  where
    this = applicationStates app

storyMode :: Application -> AppState
storyMode app = AppState $ do
    storymodeFile <- getDataFileName "manual/storyModeIntroduction"
    text <- io $ System.IO.readFile storymodeFile
    io $ setDrawingCallbackAppWidget (window app) $ Just $ render text
    io $ waitAnyKey app
    return $ applicationStates app
  where
    render text ptr = do
        clearScreen ptr
        resetMatrix ptr
        translate ptr (Position 30 40)
        drawTextBlock ptr text

-- | asks, if the user really wants to quit
quit :: Application -> AppState -> AppState
quit app parent =
    menu app (Just "quit?") (Just parent) [
        ("no", applicationStates app),
        ("yes", FinalState)
      ]

-- | select a saved level.
selectLevelPlay :: Application -> AppState -> AppState
selectLevelPlay app parent = ioAppState $ do
    levelFiles <- sort <$> filter (\ p -> takeExtension p == ".nl") <$> getDirectoryContents "."
    if null levelFiles then
        return $ menu app (Just "no levels found.") (Just parent) [("back", parent)]
      else do
        return $ menu app (Just "pick a level to play") (Just parent) $
            map (\ path -> (path, play app parent path)) levelFiles



selectLevelEdit :: Application -> AppState -> AppState
selectLevelEdit app parent = ioAppState $ do
    levelFiles <- sort <$> filter (\ p -> takeExtension p == ".nl") <$> getDirectoryContents "."
    return $ menu app (Just "pick a level to edit") (Just parent) $
        ("new level", pickNewLevel app parent) :
        map (\ path -> (path, edit app parent (path, False))) levelFiles

pickNewLevel :: Application -> AppState -> AppState
pickNewLevel app parent = AppState $ do
    pathToEmptyLevel <- getDataFileName (templateLevelsDir </> "empty.nl")
    templateLevelPaths <- filter (not . ("empty.nl" `List.isSuffixOf`)) <$>
                          getDataFiles ".nl" templateLevelsDir
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
    cmdChannel <- newChan
    setDrawingCallbackAppWidget (window app) (Just $ showProgress cmdChannel)
    grounds <- loadByFilePath file
    let mFile = if isTemplateFile then Nothing else Just file
    editorScene <- initEditorScene (allSorts app) mFile grounds
    return $ follower editorScene
  where
    showProgress cmdChannel ptr = globalCatcher $ do
        cmds <- pollChannel cmdChannel
        mapM_ id cmds
        resetMatrix ptr
        clearScreen ptr
        drawText ptr (Position 100 100) False "loading..."


