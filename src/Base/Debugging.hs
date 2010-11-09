{-# language TypeSynonymInstances #-}

module Base.Debugging (
    DebuggingCommand,
    resetDebugging,
    addDebugging,
    getDebugging,
  ) where


import Data.Initial
import Data.IORef
import Data.Monoid

import System.IO.Unsafe

import Graphics.Qt

import Utils

import Base.Constants


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
