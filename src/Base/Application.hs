
module Base.Application (
    ioAppState,
    staticConfigAppState,
    executeStates,

    menu,
    treeToMenu,
    askStringRead,
    askString,
    drawTextBlock,
    waitAnyKey,
    showText,
  ) where


import Safe

import Data.SelectTree hiding (selectPrevious, selectNext)
import qualified Data.Indexable as I

import Control.Monad
import Control.Concurrent

import Graphics.Qt

import Utils

import Base.Types
import Base.Constants
import Base.Polling
import Base.Monad
import Base.Prose
import Base.Font

import Base.Application.Menu


-- | if you don't need the M monad, just IO
ioAppState :: IO AppState -> AppState
ioAppState = AppState . io

-- | if you want to only read the configuration
staticConfigAppState :: RM AppState -> AppState
staticConfigAppState = AppState . rm2m


executeStates :: AppState -> M ()
executeStates (AppState cmd) =
    cmd >>= executeStates
executeStates FinalState = return ()


-- * widgets

-- | Gets a string from the user.
-- returns the parent if Escape is pressed.
askString :: Application_ sort -> AppState -> String -> (String -> AppState) -> AppState
askString app parent question follower = AppState $ do
    answerRef <- io $ newMVar ""
    io $ setDrawingCallbackAppWidget (window app) (Just $ render question answerRef)
    loop answerRef
  where
    loop answerRef = do
        io $ updateAppWidget $ window app
        event <- waitForAppEvent app $ keyPoller app
        case event of
            Press StartButton ->
                return parent
            Press (KeyboardButton k _) | k == Return || k == Enter ->
                io $ follower <$> readMVar answerRef
            Press (KeyboardButton k text) -> do
                io $ modifyMVar_ answerRef (\ x -> return $ modifyTextField k text x)
                loop answerRef
            _ -> loop answerRef
    render :: String -> MVar String -> Ptr QPainter -> IO ()
    render question answerRef ptr = do
        resetMatrix ptr
        clearScreen ptr
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

-- | waits for any key.
waitAnyKey :: Application_ s -> M ()
waitAnyKey app = do
    e <- waitForAppEvent app $ keyPoller app
    case e of
        Press _ -> return ()
        _ -> waitAnyKey app

drawTextBlock :: Font -> Ptr QPainter -> [Prose] -> IO ()
drawTextBlock font ptr = mapM_ $ \ line -> do
    fst (renderLine font line) ptr
    translate ptr (Position 0 (fontHeight font))

showText :: Application_ sort -> [Prose] -> AppState -> AppState
showText app text follower = AppState $ do
    io $ setDrawingCallbackAppWidget (window app) $ Just $ render text
    waitAnyKey app
    return follower
  where
    render text ptr = do
        clearScreen ptr
        resetMatrix ptr
        translate ptr (Position (fromUber 4) (fromUber 4))
        let font = alphaNumericFont $ applicationPixmaps app
        drawTextBlock font ptr text
