{-# language ScopedTypeVariables #-}

-- | The (real) main (that is, entry-) module for the game

module Game.MainLoop (
    gameLoop,
    GameState(..)
  ) where


import Data.Set (member)
import Data.Initial

import Control.Monad.State hiding ((>=>))
import Control.Concurrent
import Control.Concurrent.TickTimer
import Control.Exception

import GHC.Conc

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base.Events
import Base.Constants
import Base.Types
import Base.Application
import Base.Debugging

import Object

import Game.Scene


-- prints the version number of qt and exits
debugQtVersion :: IO ()
debugQtVersion = do
    v <- qtVersion
    putStrLn ("Qt-Version: " ++ v)

-- prints the number of HECs (see haskell concurrency)
debugNumberOfHecs :: IO ()
debugNumberOfHecs =
    putStrLn ("Number of HECs: " ++ show numCapabilities)



-- * running the state monad inside the render IO command
-- renderCallback :: Application -> IORef GameAppState -> [QtEvent] -> Ptr QPainter -> IO ()
-- renderCallback app stateRef qtEvents painter = do
--     let allEvents = toEitherList qtEvents []
-- 
--     state <- readIORef stateRef
--     ((), state') <- runStateT (renderWithState app painter) state
--     writeIORef stateRef state'

-- Application Monad and State

type AppMonad o = StateT GameState IO o

data GameState = GameState {
    cmSpace :: CM.Space,
    scene :: Scene Object_
  }

setScene :: GameState -> Scene Object_ -> GameState
setScene s x = s{scene = x}


-- | main loop for logic thread in gaming mode
-- the sceneMVar has to be empty initially.
gameLoop :: Application -> MVar (Scene Object_, DebuggingCommand) -> AppMonad AppState
gameLoop app sceneMVar = do
    initializeSceneMVar
    timer <- liftIO $ newTickTimer (stepQuantum / timeFactor)
    withAutoRepeat app $
        loop timer
  where
    loop timer = do
        liftIO $ resetDebugging

        -- input events
        controlData <- liftIO $ pollAppEvents $ keyPoller app

        -- stepping of the scene (includes rendering)
        space <- gets cmSpace
        sc <- gets scene
        sc' <- liftIO $ stepScene space controlData sc
        puts setScene sc'

        swapSceneMVar =<< liftIO getDebugging

        let startPressed = Press StartButton `elem` pressed controlData
        case mode sc' of
            LevelFinished t _ ->
                if startPressed then
                    return FinalState
                else if spaceTime sc' - t > levelEndDuration then
                    return FinalState
                else continue
            _ -> if startPressed then do
                liftIO $ putStrLn "NYI: game menu"
                return FinalState -- TODO: should be a menu
              else continue
      where
        continue = do
            liftIO $ waitTick timer
            loop timer

    initializeSceneMVar :: AppMonad ()
    initializeSceneMVar = do
        empty <- liftIO $ isEmptyMVar sceneMVar
        when (not empty) $ fail "sceneMVar has to be empty"
        s <- gets scene
        immutableCopyOfScene <- liftIO $ Game.Scene.immutableCopy s
        liftIO $ putMVar sceneMVar (immutableCopyOfScene, initial)
    swapSceneMVar :: DebuggingCommand -> AppMonad ()
    swapSceneMVar debugging= do
        s <- gets scene
        liftIO $ do
            immutableCopyOfScene <- Game.Scene.immutableCopy s
            swapMVar sceneMVar (immutableCopyOfScene, debugging)
            return ()

withAutoRepeat :: MonadIO m => Application -> m a -> m a
withAutoRepeat app cmd = do
    liftIO $ setAutoRepeat (window app) False
    a <- cmd
    liftIO $ setAutoRepeat (window app) True
    return a
