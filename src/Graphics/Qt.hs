{-# language ScopedTypeVariables #-}

module Graphics.Qt (
    module Graphics.Qt,
    module Graphics.Qt.Types,
    module Graphics.Qt.CPPWrapper,
    module Graphics.Qt.PostGUI,
    QtEvent(..),
    modifyTextField,
    Key(..),
    Ptr,
  ) where


import Data.Abelian

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception

import Graphics.Qt.Types
import Graphics.Qt.CPPWrapper
import Graphics.Qt.Events
import Graphics.Qt.PostGUI

import Foreign.Ptr

import System.Exit

import Utils


-- * entry points

withQApplication :: (Ptr QApplication -> IO a) -> IO a
withQApplication cmd = do
    app <- newQApplication
    cmd app `finally` destroyQApplication app

qtRendering :: Ptr QApplication
    -> Ptr GLContext
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

    setDrawingCallbackGLContext window (Just loop)

    showGLContext window
    code <- execQApplication app

    return $ case code of
        0 -> ExitSuccess
        c -> ExitFailure c

-- | sets a list of files that can be used as application icons (for the window manager)
setApplicationIcon :: Ptr GLContext -> [FilePath] -> IO ()
setApplicationIcon window iconPaths = do
    qIcon <- newQIcon
    mapM_ (addFileQIcon qIcon) iconPaths
    setWindowIcon window qIcon


data WindowSize = Windowed (Size QtInt) | FullScreen

setWindowSize :: Ptr GLContext -> WindowSize -> IO ()
setWindowSize win (Windowed (Size width height)) =
    resizeGLContext win width height
setWindowSize win FullScreen =
    setFullscreenGLContext win True


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


-- * convenience drawing

-- | clears the whole screen
clearScreen :: Ptr QPainter -> Color -> IO ()
clearScreen ptr color = do
    resetMatrix ptr
    windowSize <- sizeQPainter ptr
    eraseRect ptr zero windowSize color


-- * Key Polling

newtype KeyPoller = KeyPoller (TChan QtEvent)

newKeyPoller :: Ptr GLContext -> IO KeyPoller
newKeyPoller widget = do
    chan <- newTChanIO
    setKeyCallbackGLContext widget (atomically . writeTChan chan)
--     sendDebugInitials chan
    return $ KeyPoller chan

pollEvents :: KeyPoller -> IO [QtEvent]
pollEvents kp@(KeyPoller chan) = atomically inner
  where
    inner = do
        empty <- isEmptyTChan chan
        if empty then
            return []
          else do
            a <- readTChan chan
            r <- inner
            return (a : r)

waitForEvent :: KeyPoller -> IO QtEvent
waitForEvent (KeyPoller c) = atomically $ readTChan c


sendDebugInitials :: TChan QtEvent -> IO ()
sendDebugInitials c = ignore $ forkOS $ atomically $ do
    mapM_ worker signals
  where
    worker k = do
        writeTChan c (KeyPress k (text k))
        writeTChan c (KeyRelease k (text k))
    text K0 = "0"
    text K5 = "5"
    text Dot = "."
    text x = return $ head (show x)

    signals =
        play 1 ++
        []
    edit n = DownArrow : DownArrow : DownArrow : Ctrl :
             replicate n DownArrow ++ Ctrl : []
    play n =    DownArrow : Ctrl :
                replicate (n - 1) DownArrow ++ Ctrl : []
