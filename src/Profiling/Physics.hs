
module Profiling.Physics (Profiling.Physics.render, terminate) where


import Data.IORef
import Data.Time.Clock.POSIX
import Data.Abelian

import Text.Printf

import System.IO
import System.IO.Unsafe

import Physics.Chipmunk

import Graphics.Qt

import Utils

import Base


-- | time window which will be measured
profilingWindow :: Seconds
profilingWindow = 1

-- | returns the current time
getTime :: IO CpFloat
getTime = realToFrac <$> getPOSIXTime

render :: Application -> Configuration -> Ptr QPainter -> Seconds -> IO ()
render app config ptr spaceTime | physics_profiling config = do
    text <- tick spaceTime
    resetMatrix ptr
    translate ptr (Position (fromUber 1) 0)
    snd =<< Base.render ptr app config zero (False, text)
render _ _ _ _ = return ()

-- | calculate the information to be shown
tick :: Seconds -> IO Prose
tick spaceTime = do
    realTime <- getTime
    (State oldMeasureTime oldDiff oldText log) <- readIORef ref
    hPrint log realTime
    if realTime - oldMeasureTime >= profilingWindow then do
        let newDiff = realTime - spaceTime
            diffChange = (newDiff - oldDiff) / profilingWindow
            newText = pVerbatim (printf "Slowdown: %3.1f%%" (diffChange * 100))
        writeIORef ref (State realTime newDiff newText log)
        return newText
      else
        return oldText

terminate :: Configuration -> IO ()
terminate config | physics_profiling config = do
    h <- logFile <$> readIORef ref
    hFlush h
    hClose h
terminate _ = return ()

{-# NOINLINE ref #-}
ref :: IORef State
ref = unsafePerformIO $ do
    now <- getTime
    log <- openFile "physicsTimes.log" WriteMode
    newIORef (State now (now - 0) (pVerbatim "") log)

data State = State {
    oldMeasureTime :: CpFloat, -- (POSIX) time of last measurement
    oldDiff :: CpFloat, -- old difference between POSIX time and space time of the physics engine
    oldText :: Prose,
    logFile :: Handle
  }
