{-# language ScopedTypeVariables #-}

-- | The (real) main (that is, entry-) module for the game

module Game.MainLoop (
    gameLoop,
    GameState(..),
    initialStateRef,
    initialState
  ) where


import Data.IORef
import Data.Set (member)

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
import Base.Application

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

initialStateRef :: Ptr QApplication -> Ptr AppWidget -> CM.Space -> IO (Scene Object_)
    -> IO (IORef GameState)
initialStateRef app widget space scene = initialState app widget space scene >>= newIORef

initialState :: Ptr QApplication -> Ptr AppWidget -> CM.Space -> IO (Scene Object_) -> IO GameState
initialState app widget space startScene = do
    scene <- startScene
    qtime <- newQTime
    startQTime qtime
    return $ GameState space scene qtime



-- | main loop for logic thread in gaming mode
-- the sceneMVar has to be empty initially.
gameLoop :: Application -> MVar (Scene Object_) -> AppMonad AppState
gameLoop app sceneMVar = do
    initializeSceneMVar
    loop 0
  where
    loop oldTime = do
        timer_ <- gets timer
        startTime <- getTime
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
            _ -> if StartButton `member` held controlData then
                return FinalState -- TODO: should be a menu
              else do
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
waitPhysics :: Double -> AppMonad ()
waitPhysics startTime = do
    let loop n = do
            now <- getTime
            if (now - startTime < (stepQuantum * 1000)) then
                loop (n + 1)
              else
                return n
    n <- loop 0
    tickBusyWaitCounter n

timeFactor = 1.0


-- | returns the time passed since program start
getTime :: AppMonad Double
getTime = do
    qtime <- gets timer
    t <- liftIO $ elapsed qtime
    return (fromIntegral t * timeFactor)
