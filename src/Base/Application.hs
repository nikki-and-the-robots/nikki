
module Base.Application (
    appState,
    executeStates,
  ) where


import Control.Monad.State (get)

import Graphics.Qt

import Utils

import Base.Types
import Base.Configuration

import Base.Renderable.Common ()


appState :: Renderable r => r -> M AppState -> AppState
appState r = AppState (RenderableInstance r)

executeStates :: Application -> AppState -> M ()
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
executeStates app (NoGUIAppState cmd) =
    cmd >>= executeStates app
executeStates app (GameAppState cmd) =
    cmd >>= executeStates app
executeStates _ FinalAppState = return ()


