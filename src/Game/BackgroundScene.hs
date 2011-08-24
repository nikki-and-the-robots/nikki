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
waitForPressedButtonBackgroundScene :: forall a . Application -> GameState -> SceneMVar
    -> M Button
waitForPressedButtonBackgroundScene app gameState sceneMVar =
    flip evalStateT gameState $ withTimer loopSuperStep
  where
    -- | loops to perform supersteps
    loopSuperStep :: Timer -> GameMonad Button
    loopSuperStep timer = do
        performSubSteps timer subStepsPerSuperStep
        mEvent <- lift $ nextAppEvent app
        let continue = do
                io $ waitTimer timer (realToFrac (superStepQuantum / timeFactor))
                loopSuperStep timer
        case mEvent of
            Just (Press b) -> return b
            _ -> continue

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
