{-# language ScopedTypeVariables #-}

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
    logInfo ("Qt-Version: " ++ v)

-- | create AppState for game mode
gameAppState :: Application -> Bool -> GameState -> AppState
gameAppState app editorTestMode initialState = NoGUIAppState $ do
    renderState <- mkRenderState (Base.cameraStateRef initialState) (scene initialState)
    let sceneMVar_ = sceneMVar renderState
    return $ GameAppState (renderable renderState) (gameLoop app editorTestMode sceneMVar_) initialState

-- | main loop for logic thread in gaming mode
-- the sceneMVar has to be empty initially.
-- The returned AppState is somehow independent from the other AppState.
-- Returns FinalState to return to the level selection.
gameLoop :: Application -> Bool -> MVar (Scene Object_, DebuggingCommand) -> GameMonad AppState
gameLoop app editorTestMode sceneMVar =
    loop =<< io newTimer
  where
    loop :: Timer -> GameMonad AppState
    loop timer = do
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

        case sc' ^. mode of
            LevelFinished _ Failed ->
                return $ failureMenu app
            LevelFinished score Passed ->
                if editorTestMode then
                    return $ successMessage app score (Nothing, NoNewRecord, NoNewRecord)
                  else do
                    records <- io $ saveScore (levelFile sc') score
                    return $ successMessage app score records
            _ -> if isGameBackPressed (controls configuration) controlData then do
                follower <- gameAppState app editorTestMode <$> get
                return $ pauseMenu app follower 0
              else
                continue
      where
        continue = do
            io $ waitTimer timer (realToFrac (stepQuantum / timeFactor))
            loop timer

    swapSceneMVar :: DebuggingCommand -> GameMonad ()
    swapSceneMVar debugging = do
        s <- gets scene
        immutableCopyOfScene <- io $ sceneImmutableCopy s
        io $ modifyMVar_ sceneMVar (const $ return (immutableCopyOfScene, debugging))
        return ()
