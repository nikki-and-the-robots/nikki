{-# language ScopedTypeVariables #-}

-- | module to continue the physics simulation of the scene in a background thread.

module Game.BackgroundScene where


import Data.Initial

import Control.Monad.State.Strict
import Control.Concurrent

import Clocked

import Utils

import Base

import Game.Scene


-- | advances the scene until the given action returns a result
startBackgroundThread :: forall a . Application -> GameState -> SceneMVar
    -> (ControlData -> Maybe a)
    -> M a
startBackgroundThread app gameState sceneMVar logic =
    flip evalStateT gameState $ withTimer loopSuperStep
  where
    -- | loops to perform supersteps
    loopSuperStep :: Timer -> GameMonad a
    loopSuperStep timer = do
        performSubSteps timer subStepsPerSuperStep
        controlData <- lift $ pollAppEvents app
        case logic controlData of
            Nothing -> do
                io $ waitTimer timer (realToFrac (superStepQuantum / timeFactor))
                loopSuperStep timer
            Just x -> return x

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
        sc' <- io $ stepScene configuration space initial sc
        puts setScene sc'

        swapSceneMVar =<< io getDebugging

        performSubSteps timer (pred n)

    swapSceneMVar :: DebuggingCommand -> GameMonad ()
    swapSceneMVar debugging = do
        s <- gets scene
        immutableCopyOfScene <- io $ sceneImmutableCopy s
        io $ modifyMVar_ sceneMVar (const $ return (immutableCopyOfScene, debugging))
        return ()
