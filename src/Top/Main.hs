{-# language ScopedTypeVariables #-}

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
--    Most of the states will use 'setDrawingCallbackMainWindow' to set the rendering function.
--    This is the way, to let the rendering thread do stuff.
--
-- Forking takes place once in 'Top.main' forking the logic thread.

module Top.Main where


import Data.List as List
import Data.Typeable

import Text.Logging

import Control.Concurrent
import Control.Monad.Error
import Control.Monad.Reader
import Control.Exception

import System.FilePath
import System.Exit

import Graphics.Qt

import Utils

import Base

import Distribution.AutoUpdate.MenuItem

import Top.Initialisation
import Top.Menu


main :: IO ()
main = do
    configuration <- loadConfiguration
    exitWith =<< forkThreads (renderThread configuration) (logicThread configuration)

-- | Handles forking of the two main threads.
-- Creates an MVar for the one thread to initialize.
-- If one threads raises an exception, the other one will be killed.
forkThreads :: (MVar a -> IO ()) -> (a -> IO ()) -> IO ExitCode
forkThreads renderThread logicThread = do
    aRef <- newEmptyMVar
    logicExceptionRef <- newEmptyMVar
    waitRef <- newEmptyMVar
    renderTID <- myThreadId
    logicTID <- forkOS $ flip finally (putMVar waitRef ()) $ do
        a <- takeMVar aRef
        (logicThread a >> noException logicExceptionRef)
             `Control.Exception.catch` handleLogicException renderTID logicExceptionRef
    renderThread aRef
    takeMVar waitRef
    takeMVar logicExceptionRef
  where
    noException logicExceptionRef = putMVar logicExceptionRef ExitSuccess
    handleLogicException :: ThreadId -> MVar ExitCode -> SomeException -> IO ()
    handleLogicException tid logicExceptionRef (SomeException e) = do
        case cast e :: Maybe ExitCode of
            Nothing -> putMVar logicExceptionRef (ExitFailure 1)
            Just exitCode -> putMVar logicExceptionRef exitCode
        throwTo tid e


-- | Rendering thread.
-- Initialises the Application and puts it in the given MVar.
-- Enters the Qt event loop after that.
-- Displays a "loading..." message as soon as possible.
renderThread :: Configuration -> MVar Application -> IO ()
renderThread configuration appRef =
  withQApplication $ \ qApp -> do
    withMainWindow 0 (width defaultWindowSize) (height defaultWindowSize) $ \ window -> do
      paintEngine <- paintEngineTypeMainWindow window
      logg Debug ("paint engine: " ++ show paintEngine)
      flip runReaderT configuration $ withNikkiIcon window $ do
        keyPoller <- io $ newKeyPoller window
            (initial_events configuration ++ initialDebuggingSignals)
        -- loading the gui pixmaps
        withApplicationPixmaps $ \ appPixmaps -> do
          -- showing main window
          io $ do
            let windowMode = if fullscreen configuration
                  then FullScreen
                  else Windowed defaultWindowSize
            setWindowTitle window "Nikki and the Robots"
            setWindowSize window windowMode
            showLoadingScreen qApp window appPixmaps configuration
            showMainWindow window
            processEventsQApplication qApp

          -- sort loading (pixmaps and sounds)
          withApplicationPixmaps $ \ appPixmaps ->
            withAllSorts $ \ sorts ->
              withApplicationSounds $ \ appSounds -> io $ do
                autoUpdateVersionRef <- mkUpdateVersionRef window configuration
                -- put the initialised Application in the MVar
                let app :: Application
                    app = Application qApp window keyPoller
                            autoUpdateVersionRef startAppState appPixmaps
                            appSounds sorts
                putMVar appRef app
                -- will be quit by the logick thread
                exitCode <- execQApplication qApp
                when (exitCode /= 0) $
                    logg Error ("error exit code from execQApplication: " ++ show exitCode)

withNikkiIcon :: Ptr MainWindow -> RM a -> RM a
withNikkiIcon qWidget action = do
    iconPaths <- filter (("icon" `isPrefixOf`) . takeFileName) <$>
        getDataFiles pngDir (Just ".png")
    withApplicationIcon qWidget iconPaths action

-- showLoadingScreen :: Application -> Configuration -> IO ()
showLoadingScreen qApp window applicationPixmaps config = do
    let app = Application qApp window err err err applicationPixmaps err err
        err = error "uninitialised field in Application: showLoadingScreen"
    postGUI window $ setRenderingLooped window False
    io $ setRenderable app config (busyMessage (p "loading..."))


-- | Logic thread.
-- Runs the logic and the physics engine. Sets renderingCallbacks.
-- Writes the possibly modified Configuration to disk at the end.
logicThread :: Configuration -> Application -> IO ()
logicThread configuration app = flip finally quitQApplication $ do
    -- dynamic changes of the configuration take place in this thread!
    withDynamicConfiguration configuration $
        runAppState app (startAppState app)
