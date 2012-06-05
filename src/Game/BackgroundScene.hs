{-# language ScopedTypeVariables #-}

-- | module to continue the physics simulation of the scene in a background thread.

module Game.BackgroundScene where


import Data.Initial
import Data.IORef

import Control.Monad.State.Strict
import Control.Concurrent

import Clocked

import Utils

import Base

import Game.Scene


-- | Advances the scene until a key that is validated by the given predicate is pressed,
-- or the given timelimit is reached
waitForPressedButtonBackgroundScene :: forall a . Application -> IORef GameState -> SceneMVar
    -> (Button -> Bool) -> Maybe POSIXTime -> M (Maybe Button)
waitForPressedButtonBackgroundScene app gameStateRef sceneMVar buttonPred mTimeLimit = do
    startTime <- io $ getTime
    gameState <- io $ readIORef gameStateRef
    (button, gameState') <- runStateT (withTimer $ loopSuperStep startTime) gameState
    io $ writeIORef gameStateRef gameState'
    return button
  where
    -- | loops to perform supersteps
    loopSuperStep :: POSIXTime -> Timer -> GameMonad (Maybe Button)
    loopSuperStep startTime timer = do
        performSubSteps timer subStepsPerSuperStep
        mEvent <- lift $ nextAppEvent app
        let continue = do
                io $ waitTimer timer (realToFrac (superStepQuantum / timeFactor))
                loopSuperStep startTime timer
        state <- get
        case mEvent of
            Just (Press b) | buttonPred b -> return $ Just b
            _ -> do
                exceeds <- io $ exceedsTimeLimit startTime
                if exceeds
                    then return Nothing
                    else continue

    -- If waitForPressedButtonBackgroundScene exceeded the given time limit.
    exceedsTimeLimit :: POSIXTime -> IO Bool
    exceedsTimeLimit startTime = case mTimeLimit of
        Nothing -> return False
        Just timeLimit -> do
            now <- getTime
            return (now - startTime >= timeLimit)

    -- | performs n substeps. Aborts in case of level end. Returns (Just AppState) in
    -- that case. Nothing means, the game should continue.
    performSubSteps :: Timer -> Int -> GameMonad ()
    performSubSteps timer 0 = return ()
    performSubSteps timer n = do
        io $ resetDebugging

        -- stepping of the scene (includes rendering)
        space <- gets cmSpace
        sc <- gets scene
        configuration <- lift get
        sc' <- io $ stepScene app configuration space initial sc
        puts setScene sc'

        swapSceneMVar =<< io getDebugging

        performSubSteps timer (pred n)

    swapSceneMVar :: DebuggingCommand -> GameMonad ()
    swapSceneMVar debugging = do
        s <- gets scene
        immutableCopyOfScene <- io $ sceneImmutableCopy s
        io $ modifyMVar_ sceneMVar (const $ return (immutableCopyOfScene, debugging))
        return ()
