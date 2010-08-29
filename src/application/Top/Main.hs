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


import Data.SelectTree
import Data.Indexable as I
import Data.List

import Control.Concurrent

import System.FilePath
import System.IO
import System.Exit
import System.Directory

import GHC.Conc

import Physics.Chipmunk

import Graphics.Qt

import Utils

import Base.GlobalCatcher
import Base.Types hiding (menu)
import Base.Configuration

import Object

import Game.MainLoop

import Editor.Scene (initEditorScene)

import Top.Pickle
import Top.Initialisation
import Top.Editor (editLevel)
import Top.Game (playLevel)
import Top.Application


-- prints the number of HECs (see haskell concurrency)
debugNumberOfHecs :: IO ()
debugNumberOfHecs =
    putStrLn ("Number of HECs: " ++ show numCapabilities)

main = globalCatcher $ do

--     debugNumberOfHecs

    hSetBuffering stdout NoBuffering
    putStrLn "\nstarted..."

    -- qt initialisation
    qApp <- newQApplication
    window <- newAppWidget 0
    keyPoller <- newKeyPoller window

    -- sort loading (pixmaps and sounds)
    sorts <- getAllSorts sortLoaders

    -- start state logick
    let app = Application qApp window keyPoller sorts
    -- there are two main threads:
    -- this is the logick [sick!] thread
    forkOS $ globalCatcher $ do
        executeStates (applicationStates app)
        quitQApplication

    -- start app
    setWindowSize window (windowSize Base.Configuration.development)
    showAppWidget window
    -- this is the rendering thread
    code <- execQApplication qApp

    case code of
        0 -> exitWith ExitSuccess


getAllSorts :: [IO [Sort_]] -> IO (SelectTree Sort_)
getAllSorts sortLoaders = do
    sorts <- concat <$> mapM id sortLoaders
    return $ Node "editor-Tiles" (I.fromList $ map Leaf sorts) 0



-- * states

-- | top level application state (main menu)
applicationStates :: Application -> AppState
applicationStates app =
    menu app Nothing Nothing [
        ("play", selectLevelPlay app this),
        ("edit", selectLevelEdit app this),
        ("quit", quit app this)
      ]
  where
    this = applicationStates app

-- | asks, if the user really wants to quit
quit :: Application -> AppState -> AppState
quit app parent =
    menu app (Just "quit?") (Just parent) [
        ("no", applicationStates app),
        ("yes", FinalState)
      ]

-- | select a saved level.
selectLevelPlay :: Application -> AppState -> AppState
selectLevelPlay app parent = AppState $ do
    levelFiles <- sort <$> filter (\ p -> takeExtension p == ".nl") <$> getDirectoryContents "."
    if null levelFiles then
        return $ menu app (Just "no levels found.") (Just parent) [("back", parent)]
      else do
        let follower = Top.Main.playLevel app parent
        return $ menu app (Just "pick a level to play") (Just parent) $
            map (\ p -> (p, loadingEditorScene app p follower)) levelFiles

-- | load a level, got to playing state afterwards
-- This AppState involves is a hack to do things from the logic thread 
-- in the rendering thread. Cause Qt's pixmap loading is not threadsafe.
loadingEditorScene :: Application -> FilePath -> (EditorScene Sort_ -> AppState) -> AppState
loadingEditorScene app file follower = AppState $ do
    cmdChannel <- newChan
    setDrawingCallbackAppWidget (window app) (Just $ showProgress cmdChannel)
    grounds <- loadByFilePath file
    editorScene <- initEditorScene (sorts app) (Just (file, grounds))
    return $ follower editorScene
  where
    showProgress cmdChannel ptr = globalCatcher $ do
        cmds <- pollChannel cmdChannel
        mapM_ id cmds
        resetMatrix ptr
        clearScreen ptr
        drawText ptr (Position 100 100) False "loading..."

playLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
playLevel app parent editorScene = AppState $ do
    let scene :: (Space -> IO (Scene Object_)) = flip initScene (editorObjects editorScene)
    state <- initialState (application app) (window app) scene
    return $ Top.Game.playLevel app state parent


selectLevelEdit :: Application -> AppState -> AppState
selectLevelEdit app parent = AppState $ do
    levelFiles <- sort <$> filter (\ p -> takeExtension p == ".nl") <$> getDirectoryContents "."
    return $ menu app (Just "pick a level to edit") (Just parent) $
        ("new level", pickNewLevel) :
        map (\ p -> (p, loadingEditorScene app p (editLevel app parent))) levelFiles

pickNewLevel :: AppState
pickNewLevel = error "pickNewLevel"

