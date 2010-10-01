
module Graphics.Qt (
    module Graphics.Qt.Types,
    module Graphics.Qt.CPPWrapper,
    module Graphics.Qt,
    QtEvent(..),
    modifyTextField,
    Key(..),
    Ptr,
  ) where


import Data.Abelian

import Control.Concurrent

import Graphics.Qt.Types
import Graphics.Qt.CPPWrapper
import Graphics.Qt.Events

import Foreign.Ptr

import System.Exit

import Utils


-- * entry points

qtRendering :: Ptr QApplication
    -> Ptr AppWidget
    -> String
    -> WindowSize
    -> ([QtEvent] -> Ptr QPainter -> IO ())
    -> (IO () -> IO ())
    -> IO ExitCode
qtRendering app window title windowSize renderCmd catcher = do
    setWindowTitle window title
    setWindowSize window windowSize
    keyPoller <- newKeyPoller window

    let loop qPainter = catcher $ do
            events <- pollEvents keyPoller
            renderCmd events qPainter

    setDrawingCallbackAppWidget window (Just loop)

    showAppWidget window
    code <- execQApplication app

    return $ case code of
        0 -> ExitSuccess
        c -> ExitFailure c


data WindowSize = Windowed (Size QtInt) | FullScreen

setWindowSize :: Ptr AppWidget -> WindowSize -> IO ()
setWindowSize win (Windowed (Size width height)) =
    resizeAppWidget win width height
setWindowSize win FullScreen =
    setFullscreenAppWidget win True


-- * Colors

red :: Color
red = opaqueColor 1 0 0

black :: Color
black = opaqueColor 0 0 0

white :: Color
white = opaqueColor 1 1 1

opaqueColor :: Double -> Double -> Double -> Color
opaqueColor r g b =
    QtColor
        (doubleToByteInt r)
        (doubleToByteInt g)
        (doubleToByteInt b)
        (doubleToByteInt 1)

doubleToByteInt :: Double -> QtInt
doubleToByteInt d | d >= 0 && d <= 1 =
    truncate (d * 255)


-- * convenience drawing

-- | clears the whole screen
clearScreen :: Ptr QPainter -> IO ()
clearScreen ptr = do
    resetMatrix ptr
    windowSize <- sizeQPainter ptr
    eraseRect ptr zero windowSize (QtColor 0 0 0 255)


-- * Key Polling

newtype KeyPoller = KeyPoller (Chan QtEvent)

newKeyPoller :: Ptr AppWidget -> IO KeyPoller
newKeyPoller widget = do
    chan <- newChan
    setKeyCallbackAppWidget widget (writeChan chan)
--     sendDebugInitials chan
    return $ KeyPoller chan

pollEvents :: KeyPoller -> IO [QtEvent]
pollEvents kp@(KeyPoller chan) = do
    empty <- isEmptyChan chan
    if empty then
        return []
      else do
        a <- readChan chan
        r <- pollEvents kp
        return (a : r)

waitForEvent :: KeyPoller -> IO QtEvent
waitForEvent (KeyPoller c) = readChan c


sendDebugInitials :: Chan QtEvent -> IO ()
sendDebugInitials c = ignore $ forkOS $ do
    mapM_ worker (saveNewLevel)
  where
    worker k = do
        threadDelay $ round (0.1 * 10 ^ 6)
        writeChan c (KeyPress k (return $ head $ show k))
        writeChan c (KeyRelease k (return $ head $ show k))

    editFirstLevel = [DownArrow, Ctrl, DownArrow, Ctrl]
    saveNewLevel = [DownArrow, Ctrl, Ctrl, Ctrl, Ctrl, DownArrow, DownArrow, D, Ctrl, RightArrow, RightArrow, D, D, D, D, Ctrl, Escape, DownArrow, DownArrow, DownArrow, DownArrow, DownArrow, Ctrl]
    selectionMode = editFirstLevel ++ [Space]
    playFirstLevel = [Ctrl, Ctrl]
