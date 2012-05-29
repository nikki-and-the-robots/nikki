{-# language NamedFieldPuns #-}

-- | The (real) main (that is, entry-) module for the game

module Game.MainLoop (
    gameAppState,
    gameLoop,
    GameState(..)
  ) where


import Text.Logging

import Control.Monad.State hiding ((>=>))
import Control.Monad.CatchIO (MonadCatchIO, bracket)
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
gameAppState :: Application -> Parent -> Bool -> GameState -> AppState
gameAppState app parent editorTestMode initialState = NoGUIAppState $ do
    renderState <- mkRenderState (Base.cameraStateRef initialState) (scene initialState)
    rm2m $ startGameBackgroundMusic $ levelMetaData $ levelFile $ scene initialState
    return $ GameAppState
        (renderable renderState)
        (gameLoop app parent editorTestMode renderState)
        initialState


-- | main loop for logic thread in gaming mode
-- the sceneMVar has to be empty initially.
-- The returned AppState is somehow independent from the other AppState.
gameLoop :: Application -> Parent -> Bool -> RenderStateRefs -> GameMonad AppState
gameLoop app parent editorTestMode rsr@RenderStateRefs{sceneMVar} =
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
        sc' <- io $ stepScene app configuration space controlData sc
        puts setScene sc'

        swapSceneMVar =<< io getDebugging

        state <- get

        case sc' ^. mode of

            LevelFinished score result -> do
                io $ stopGameBackgroundMusic
                records <- if editorTestMode then
                                return (Nothing, NoNewRecord, NoNewRecord)
                             else
                                io $ saveScore (levelFile sc') score
                return $ Just $ case result of
                    Failed ->
                        failureMenu app parent rsr state
                    Passed ->
                        successMessage app parent rsr state score records
            _ -> if isGameBackPressed (configuration ^. controls) controlData then 
                if editorTestMode then do
                    gameState <- get
                    return $ Just $ freeGameState gameState parent
                  else do
                    -- saving a highscore of Score_1_Tried in case the level gets aborted
                    io $ saveScore (levelFile sc') Score_1_Tried
                    io $ pauseGameBackgroundMusic
                    continueLevel <- gameAppState app parent editorTestMode <$> get
                    gameState <- get
                    return $ Just $ pauseMenu app parent continueLevel gameState 0
              else
                -- continuing performing substeps
                performSubSteps timer (pred n)

    swapSceneMVar :: DebuggingCommand -> GameMonad ()
    swapSceneMVar debugging = do
        s <- gets scene
        immutableCopyOfScene <- io $ sceneImmutableCopy s
        io $ modifyMVar_ sceneMVar (const $ return (immutableCopyOfScene, debugging))
        return ()
