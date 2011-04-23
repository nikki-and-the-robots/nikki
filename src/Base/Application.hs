
module Base.Application (
    appState,
    ioAppState,
    staticConfigAppState,
    executeStates,

    askStringRead,
    askString,
  ) where


import Safe

import Control.Concurrent

import Graphics.Qt

import Utils

import Base.Types
import Base.Polling
import Base.Monad

import Base.Renderable.Common


appState :: Renderable r => r -> M AppState -> AppState
appState r = AppState (RenderableInstance r)

-- | if you don't need the M monad, just IO
ioAppState :: Renderable r => r -> IO AppState -> AppState
ioAppState r action = AppState (RenderableInstance r) (io action)

-- | if you want to only read the configuration
staticConfigAppState :: RM AppState -> AppState
staticConfigAppState = AppState (rt "staticConfigAppState") . rm2m


executeStates :: Application_ s -> AppState -> M ()
executeStates app (AppState renderable cmd) = do
    io $ setDrawingCallbackGLContext (window app) (Just renderCallback)
    cmd >>= executeStates app
  where
    renderCallback :: Ptr QPainter -> IO ()
    renderCallback ptr = do
        size <- fmap fromIntegral <$> sizeQPainter ptr
        resetMatrix ptr
        render ptr app size renderable
executeStates _ FinalState = return ()


-- * widgets

-- | Gets a string from the user.
-- returns the parent if Escape is pressed.
askString :: Application_ sort -> AppState -> String -> (String -> AppState) -> AppState
askString app parent question follower = AppState (rt "askString") $ do
    answerRef <- io $ newMVar ""
    io $ setDrawingCallbackGLContext (window app) (Just $ render question answerRef)
    loop answerRef
  where
    loop answerRef = do
        io $ updateGLContext $ window app
        event <- waitForAppEvent app
        case event of
            Press e | isStart e ->
                return parent
            Press e | isEnterOrReturn e ->
                io $ follower <$> readMVar answerRef
            Press (KeyboardButton k text) -> do
                io $ modifyMVar_ answerRef (\ x -> return $ modifyTextField k text x)
                loop answerRef
            _ -> loop answerRef
    render :: String -> MVar String -> Ptr QPainter -> IO ()
    render question answerRef ptr = do
        resetMatrix ptr
        clearScreen ptr backgroundColor
        setPenColor ptr white 1

        let nextY = translate ptr (Position 0 20)
        nextY

        answer <- readMVar answerRef
        drawText ptr (Position 10 0) False (question ++ ": " ++ answer)

-- | Like askString, but reads (parses with Read) the given String. Asks again, if not parsable.
askStringRead :: Read a => Application_ sort -> AppState -> String -> (a -> AppState) -> AppState
askStringRead app parent question follower =
    askString app parent question wrapper
  where
    wrapper :: String -> AppState
    wrapper s = case readMay s of
        Nothing -> askStringRead app parent question follower -- try again
        Just r -> follower r
