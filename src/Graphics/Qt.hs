{-# language ScopedTypeVariables #-}

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
import Control.Exception

import Graphics.Qt.Types
import Graphics.Qt.CPPWrapper
import Graphics.Qt.Events

import Foreign.Ptr

import System.Exit

import Utils


-- * entry points

withQApplication :: (Ptr QApplication -> IO a) -> IO a
withQApplication cmd = do
    app <- newQApplication
    cmd app `finally` destroyQApplication app

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

-- | sets a list of files that can be used as application icons (for the window manager)
setApplicationIcon :: Ptr AppWidget -> [FilePath] -> IO ()
setApplicationIcon window iconPaths = do
    qIcon <- newQIcon
    mapM_ (addFileQIcon qIcon) iconPaths
    setWindowIcon window qIcon


data WindowSize = Windowed (Size QtInt) | FullScreen

setWindowSize :: Ptr AppWidget -> WindowSize -> IO ()
setWindowSize win (Windowed (Size width height)) =
    resizeAppWidget win width height
setWindowSize win FullScreen =
    setFullscreenAppWidget win True


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

transparent :: Color = QtColor 0 0 0 0


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

byteIntToDouble :: QtInt -> Double
byteIntToDouble x | x >= 0 && x <= 255 =
    fromIntegral x / 255

modifyAlpha :: (Double -> Double) -> Color -> Color
modifyAlpha f (QtColor r g b a) =
    QtColor r g b (doubleToByteInt $ f $ byteIntToDouble a)


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
    mapM_ worker signals
  where
    worker k = do
        threadDelay $ round (0.1 * 10 ^ 6)
        writeChan c (KeyPress k (text k))
        writeChan c (KeyRelease k (text k))
    text K0 = "0"
    text K5 = "5"
    text Dot = "."
    text x = return $ head (show x)

    signals =   play 1 ++
--                 replicate 6 RightArrow ++ replicate 2 DownArrow ++
--                 Escape : Ctrl :
                []
    edit n = DownArrow : DownArrow : DownArrow : Ctrl :
             replicate n DownArrow ++ Ctrl : []
    play n =    DownArrow : Ctrl :
                replicate (n - 1) DownArrow ++ Ctrl : []
