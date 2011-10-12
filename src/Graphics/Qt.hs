{-# language ScopedTypeVariables #-}

module Graphics.Qt (
    module Graphics.Qt,
    module Graphics.Qt.Types,
    module Graphics.Qt.CPPWrapper,
    QtEvent(..),
    modifyTextField,
    Key(..),
    keyDescription,
    QKeyboardModifier(..),
    Ptr,
  ) where


import Data.Abelian
import Data.Set

import Control.Monad.CatchIO
import Control.Concurrent

import Graphics.Qt.Types
import Graphics.Qt.CPPWrapper
import Graphics.Qt.Events

import Foreign.Ptr

import System.Random
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


-- * Colors

black :: Color = opaqueColor 0 0 0
white :: Color = opaqueColor 1 1 1
red :: Color = opaqueColor 1 0 0
green :: Color = opaqueColor 0 1 0
blue :: Color = opaqueColor 0 0 1
yellow :: Color = opaqueColor 1 1 0
magenta :: Color = opaqueColor 1 0 1
cyan :: Color = opaqueColor 0 1 1

signalRed :: Color = opaqueColor 1 0.216 0.216
lightYellow :: Color = opaqueColor 1 1 0.216
pink :: Color = opaqueColor 1 0.196 0.588
orange :: Color = opaqueColor 1 0.5 0
lightBlue :: Color = opaqueColor 0.28 0.63 0.79
lightGreen :: Color = opaqueColor 0.66 1 0.66
darkGrey :: Color = QtColor 64 64 64 255
turquoise :: Color = QtColor 72 214 242 255

transparent :: Color = QtColor 0 0 0 0


randomColor :: IO Color
randomColor =
    QtColor <$> randByte <*> randByte <*> randByte <*> randByte
  where
    randByte = randomRIO (0, 255)


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
pollEvents kp@(KeyPoller chan) =
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
    threadDelay (5 * 10 ^ 6)
    mapM_ worker signals
  where
    worker k = do
        threadDelay $ round (0.8 * 10 ^ 6)
        writeChan c (KeyPress k (text k) empty)
        writeChan c (KeyRelease k (text k) empty)
    text K0 = "0"
    text K5 = "5"
    text Dot = "."
    text x = return $ head (show x)
