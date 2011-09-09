{-# language NamedFieldPuns #-}

-- | The (real) main (that is, entry-) module for the game

module Game.MainLoop (
    gameAppState,
    gameLoop,
    GameState(..)
  ) where


import Text.Logging

import Control.Monad.State hiding ((>=>))
import Control.Concurrent

import Clocked

import Graphics.Qt

import Utils

import Base

import Game.Scene
import Game.Menus


-- prints the version number of qt and exits
debugQtVersion :: IO ()
debugQtVersion = do
    v <- qtVersion
    logg Info ("Qt-Version: " ++ v)

-- | create AppState for game mode
gameAppState :: Application -> Bool -> GameState -> AppState
gameAppState app editorTestMode initialState = NoGUIAppState $ do
    renderState <- mkRenderState (Base.cameraStateRef initialState) (scene initialState)
    return $ GameAppState
        (renderable renderState)
        (gameLoop app editorTestMode renderState)
        initialState

-- | main loop for logic thread in gaming mode
-- the sceneMVar has to be empty initially.
-- The returned AppState is somehow independent from the other AppState.
-- Returns FinalState to return to the level selection.
gameLoop :: Application -> Bool -> RenderStateRefs -> GameMonad AppState
gameLoop app editorTestMode rsr@RenderStateRefs{sceneMVar} =
    withTimer loopSuperStep
  where
    -- | loops to perform supersteps
    loopSuperStep :: Timer -> GameMonad AppState
    loopSuperStep timer = do
        m <- performSubSteps timer subStepsPerSuperStep
        case m of
            Nothing -> do
                io $ waitTimer timer (realToFrac (superStepQuantum / timeFactor))
                loopSuperStep timer
            Just r -> return r

    -- | performs n substeps. Aborts in case of level end. Returns (Just AppState) in
    -- that case. Nothing means, the game should continue.
    performSubSteps :: Timer -> Int -> GameMonad (Maybe AppState)
    performSubSteps timer 0 = return Nothing
    performSubSteps timer n = do
        io $ resetDebugging

        -- input events
        controlData <- lift $ pollAppEvents app

        -- stepping of the scene (includes rendering)
        space <- gets cmSpace
        sc <- gets scene
        configuration <- lift get
        sc' <- io $ stepScene configuration space controlData sc
        puts setScene sc'

        swapSceneMVar =<< io getDebugging

        state <- get

        case sc' ^. mode of
            LevelFinished _ Failed ->
                return $ Just $ failureMenu app rsr state
            LevelFinished score Passed ->
                if editorTestMode then
                    return $ Just $ successMessage app rsr state score
                                        (Nothing, NoNewRecord, NoNewRecord)
                  else do
                    records <- io $ saveScore (levelFile sc') score
                    return $ Just $ successMessage app rsr state score records
            _ -> if isGameBackPressed (configuration ^. controls) controlData then 
                if editorTestMode then
                    return $ Just FinalAppState
                  else do
                    follower <- gameAppState app editorTestMode <$> get
                    return $ Just $ pauseMenu app follower 0
              else
                -- continuing performing substeps
                performSubSteps timer (pred n)

    swapSceneMVar :: DebuggingCommand -> GameMonad ()
    swapSceneMVar debugging = do
        s <- gets scene
        immutableCopyOfScene <- io $ sceneImmutableCopy s
        io $ modifyMVar_ sceneMVar (const $ return (immutableCopyOfScene, debugging))
        return ()
