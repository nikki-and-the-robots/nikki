
module Base.Application (
    appState,
    runAppState,
    setRenderable,
  ) where


import Control.Monad.State.Strict (get, evalStateT)

import Graphics.Qt

import Utils

import Base.Types

import Base.Renderable.Common ()


appState :: Renderable r => r -> M AppState -> AppState
appState r = AppState (RenderableInstance r)


runAppState :: Application -> AppState -> M ()
runAppState app (AppState renderable cmd) = do
    io $ postGUI (window app) $ setRenderingLooped (window app) False
    io $ postGUI (window app) $ setArrowAutoRepeat (window app) True
    config <- get
    io $ setRenderable app config renderable
    cmd >>= runAppState app
runAppState app (AppStateLooped renderable cmd) = do
    io $ postGUI (window app) $ setRenderingLooped (window app) True
    io $ postGUI (window app) $ setArrowAutoRepeat (window app) True
    config <- get
    io $ setRenderable app config renderable
    cmd >>= runAppState app
runAppState app (NoGUIAppState cmd) = do
    io $ postGUI (window app) $ setRenderingLooped (window app) False
    io $ setDrawingCallbackMainWindow (window app) Nothing
    cmd >>= runAppState app
runAppState app (GameAppState renderable cmd initialGameState) = do
    io $ postGUI (window app) $ setRenderingLooped (window app) True
    io $ postGUI (window app) $ setArrowAutoRepeat (window app) False
    config <- get
    io $ setRenderable app config renderable
    follower <- evalStateT cmd initialGameState
    runAppState app follower
runAppState app (UnManagedAppState cmd) = do
    io $ postGUI (window app) $ setRenderingLooped (window app) False
    io $ postGUI (window app) $ setArrowAutoRepeat (window app) True
    cmd >>= runAppState app
runAppState _ FinalAppState = return ()

setRenderable app config renderable = do
    setDrawingCallbackMainWindow (window app) (Just renderCallback)
  where
    renderCallback :: Ptr QPainter -> IO ()
    renderCallback ptr = do
        size <- io $ sizeQPainter ptr
        io $ resetMatrix ptr
        snd =<< render ptr app config size renderable
