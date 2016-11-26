{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Graphics.Qt (
    module Graphics.Qt,
    module Graphics.Qt.Types,
    module Graphics.Qt.Colors,
    module Graphics.Qt.Dimension,
    module Graphics.Qt.CPPWrapper,
    QtEvent(..),
    modifyTextField,
    Key(..),
    keyDescription,
    QKeyboardModifier(..),
    Ptr,
    ForeignPtr,
  ) where


import Data.Abelian
import Data.Set

import Control.Monad.CatchIO
import Control.Concurrent

import Graphics.Qt.Types
import Graphics.Qt.Colors
import Graphics.Qt.Dimension
import Graphics.Qt.CPPWrapper
import Graphics.Qt.Events

import Foreign.Ptr
import Foreign.ForeignPtr

import System.Exit

import Utils


-- * entry points

qtRendering :: Ptr QApplication
    -> Ptr MainWindow
    -> String
    -> WindowSize
    -> ([QtEvent] -> Ptr QPainter -> IO ())
    -> (IO () -> IO ())
    -> [Key]
    -> IO ExitCode
qtRendering app window title windowSize renderCmd catcher initialSignals = do
    setWindowTitle window title
    setWindowSize window windowSize
    keyPoller <- newKeyPoller window initialSignals

    let loop qPainter = catcher $ do
            events <- pollEvents keyPoller
            renderCmd events qPainter

    setDrawingCallbackMainWindow window (Just loop)

    showMainWindow window
    code <- execQApplication app

    return $ case code of
        0 -> ExitSuccess
        c -> ExitFailure c

-- | sets a list of files that can be used as application icons (for the window manager)
withApplicationIcon :: MonadCatchIO m => Ptr MainWindow -> [FilePath] -> m a -> m a
withApplicationIcon window iconPaths action =
    withQIcon $ \ qIcon -> do
        io $ mapM_ (addFileQIcon qIcon) iconPaths
        io $ setWindowIcon window qIcon
        action


data WindowSize = Windowed (Size QtInt) | FullScreen

setWindowSize :: Ptr MainWindow -> WindowSize -> IO ()
setWindowSize win (Windowed (Size width height)) =
    resizeMainWindow win width height
setWindowSize win FullScreen =
    setFullscreenMainWindow win True


-- * matrix

type Matrix = Ptr QTransform

recoverMatrix :: Ptr QPainter -> IO () -> IO ()
recoverMatrix ptr action =
    withMatrix ptr $ \ matrix -> do
        action
        setMatrix ptr matrix


-- * convenience drawing

-- | clears the whole screen
clearScreen :: Ptr QPainter -> Color -> IO ()
clearScreen ptr color = do
    resetMatrix ptr
    windowSize <- sizeQPainter ptr
    fillRect ptr zero windowSize color


-- * Key Polling

newtype KeyPoller = KeyPoller (Chan QtEvent)

newKeyPoller :: Ptr MainWindow -> [Key] -> IO KeyPoller
newKeyPoller widget signals = do
    chan <- newChan
    setKeyCallbackMainWindow widget (writeChan chan)
    sendInitialSignals chan signals
    return $ KeyPoller chan

pollEvents :: KeyPoller -> IO [QtEvent]
pollEvents (KeyPoller chan) =
    inner
  where
    inner = do
        empty <- isEmptyChan chan
        if empty then
            return []
          else do
            a <- readChan chan
            r <- inner
            return (a : r)

waitForEvent :: KeyPoller -> IO QtEvent
waitForEvent (KeyPoller c) = readChan c

-- | This is for development
sendInitialSignals :: Chan QtEvent -> [Key] -> IO ()
sendInitialSignals c signals = ignore $ forkOS $ do
    threadDelay (5 * 10 ^ (6 :: Int))
    mapM_ worker signals
  where
    worker k = do
        threadDelay $ round ((0.8 :: Double) * 10 ^ (6 :: Int))
        writeChan c (KeyPress k (text k) empty)
        writeChan c (KeyRelease k (text k) empty)
    text K0 = "0"
    text K5 = "5"
    text Dot = "."
    text x = return $ head (show x)
