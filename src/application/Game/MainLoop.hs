{-# language ScopedTypeVariables #-}

-- | The (real) main (that is, entry-) module for the game

module Game.MainLoop (
    gameLoop,
    GameState(..),
    initialStateRef,
    initialState
  ) where


import Data.IORef

import Control.Monad.State hiding ((>=>))
import Control.Concurrent

import GHC.Conc

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base.Events
import Base.Constants
import Base.Types
import Base.PhysicsProfiling

import Object

import Game.Scene


-- prints the version number of qt and exits
debugQtVersion :: IO ()
debugQtVersion = do
    v <- qVersion
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
    scene :: Scene Object_,
    timer :: Ptr QTime
  }

setScene :: GameState -> Scene Object_ -> GameState
setScene s x = s{scene = x}

initialStateRef :: Ptr QApplication -> Ptr AppWidget -> (CM.Space -> IO (Scene Object_))
    -> IO (IORef GameState)
initialStateRef app widget scene = initialState app widget scene >>= newIORef

initialState :: Ptr QApplication -> Ptr AppWidget -> (CM.Space -> IO (Scene Object_)) -> IO GameState
initialState app widget startScene = do
    cmSpace <- initSpace gravity
    scene <- startScene cmSpace
    qtime <- newQTime
    startQTime qtime
    return $ GameState cmSpace scene qtime



-- | main loop for logic thread in gaming mode
-- the sceneMVar has to be empty initially.
gameLoop :: Application -> MVar (Scene Object_) -> AppMonad AppState
gameLoop app sceneMVar = do
    initializeSceneMVar
    loop 0
  where
    loop oldTime = do
        timer_ <- gets timer
        startTime <- liftIO $ elapsed timer_
--         liftIO $ print (startTime - oldTime)
        -- input events
        controlData <- liftIO $ pollAppEvents $ keyPoller app

        -- stepping of the scene (includes rendering)
        space <- gets cmSpace
        sc <- gets scene
        sc' <- liftIO $ stepScene space controlData sc
        puts setScene sc'

        swapSceneMVar

        case mode sc' of
            LevelFinished _ x -> return FinalState
            _ -> do
                waitPhysics startTime
                loop startTime

    initializeSceneMVar :: AppMonad ()
    initializeSceneMVar = do
        empty <- liftIO $ isEmptyMVar sceneMVar
        when (not empty) $ fail "sceneMVar has to be empty"
        s <- gets scene
        immutableCopyOfScene <- liftIO $ Game.Scene.immutableCopy s
        liftIO $ putMVar sceneMVar immutableCopyOfScene
    swapSceneMVar :: AppMonad ()
    swapSceneMVar = do
        s <- gets scene
        liftIO $ do
            immutableCopyOfScene <- Game.Scene.immutableCopy s
            swapMVar sceneMVar immutableCopyOfScene
            return ()


-- | Waits till the real world catches up with the simulation.
-- Since 'threadDelay' seems to be far to inaccurate, we have a busy wait :(
-- TODO
waitPhysics :: Int -> AppMonad ()
waitPhysics startTime = gets timer >>= \ timer_ -> liftIO $ do
    let loop n = do
            now <- elapsed timer_
            if (now - startTime < round (stepQuantum * 1000)) then
                loop (n + 1)
              else
                return n
    n <- loop 0
    tickBusyWaitCounter n




-- | returns the time passed since program start
getSecs :: AppMonad Double
getSecs = do
    qtime <- gets timer
    time <- liftIO $ elapsed qtime
    return (fromIntegral time / 10 ^ 3)
