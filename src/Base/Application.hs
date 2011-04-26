
module Base.Application (
    appState,
    ioAppState,
    executeStates,
  ) where


import Safe

import Control.Concurrent
import Control.Monad.State (get)

import Graphics.Qt

import Utils

import Base.Types
import Base.Configuration
import Base.Polling
import Base.Monad

import Base.Renderable.Common


appState :: Renderable r => r -> M AppState -> AppState
appState r = AppState (RenderableInstance r)

-- | if you don't need the M monad, just IO
ioAppState :: Renderable r => r -> IO AppState -> AppState
ioAppState r action = AppState (RenderableInstance r) (io action)

executeStates :: Application_ s -> AppState -> M ()
executeStates app (AppState renderable cmd) = do
    config <- get
    io $ setDrawingCallbackGLContext (window app) (Just $ renderCallback config)
    cmd >>= executeStates app
  where
    renderCallback :: Configuration -> Ptr QPainter -> IO ()
    renderCallback config ptr = do
        size <- io $ fmap fromIntegral <$> sizeQPainter ptr
        io $ resetMatrix ptr
        snd =<< render ptr app config size renderable
executeStates app (NoGUIState cmd) =
    cmd >>= executeStates app
executeStates _ FinalState = return ()


