{-# language TypeSynonymInstances, FlexibleInstances #-}

module Base.Debugging (
    DebuggingCommand,
    resetDebugging,
    addDebugging,
    getDebugging,

    debugPoint,
    debugLine,
  ) where


import Data.Initial
import Data.IORef

import System.IO.Unsafe

import Graphics.Qt

import Physics.Chipmunk

import Base.Types


type DebuggingCommand = (Ptr QPainter -> Offset Double -> IO ())

instance Initial DebuggingCommand where
    initial _ _ = return ()

(<>) :: DebuggingCommand -> DebuggingCommand -> DebuggingCommand
a <> b = \ ptr offset -> a ptr offset >> b ptr offset

{-# NOINLINE debuggingCommandRef #-}
debuggingCommandRef :: IORef DebuggingCommand
debuggingCommandRef = unsafePerformIO $ newIORef initial

resetDebugging :: IO ()
resetDebugging = writeIORef debuggingCommandRef initial

addDebugging :: DebuggingCommand -> IO ()
addDebugging cmd = modifyIORef debuggingCommandRef (<> cmd)

getDebugging :: IO DebuggingCommand
getDebugging = readIORef debuggingCommandRef

-- * convenience

debugPoint :: Color -> Vector -> IO ()
debugPoint color p = addDebugging $ \ ptr offset -> do
    resetMatrix ptr
    translate ptr offset
    setPenColor ptr color 2
    drawCircle ptr (vector2position p) 5

debugLine :: Color -> Vector -> Vector -> IO ()
debugLine color a b = addDebugging $ \ ptr offset -> do
    resetMatrix ptr
    translate ptr offset
    setPenColor ptr color 4
    drawLine ptr (vector2position a) (vector2position b)
