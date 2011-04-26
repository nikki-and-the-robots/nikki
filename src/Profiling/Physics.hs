
module Profiling.Physics (render) where


import Data.IORef
import Data.Time.Clock.POSIX

import Text.Printf

import System.IO.Unsafe

import Physics.Chipmunk

import Graphics.Qt

import Utils

import Base hiding (render)


-- | time window which will be measured
profilingWindow :: Seconds
profilingWindow = 1

-- | returns the current time
getTime :: IO CpFloat
getTime = realToFrac <$> getPOSIXTime

render :: Application_ s -> Configuration -> Ptr QPainter -> Seconds -> IO ()
render app config ptr spaceTime | physics_profiling config = do
    text <- tick spaceTime
    let font = alphaNumericFont $ applicationPixmaps app
    resetMatrix ptr
    translate ptr (Position (fromUber 1) 0)
    todo
--     ignore $ renderLineSimple font Nothing white text ptr
render _ _ _ _ = return ()

-- | calculate the information to be shown
tick :: Seconds -> IO Prose
tick spaceTime = do
    realTime <- getTime
    (State oldMeasureTime oldDiff oldText) <- readIORef ref
    if realTime - oldMeasureTime >= profilingWindow then do
        let newDiff = realTime - spaceTime
            diffChange = (newDiff - oldDiff) / profilingWindow
            newText = pVerbatim (printf "Slowdown: %3.1f%%" (diffChange * 100))
        writeIORef ref (State realTime newDiff newText)
        return newText
      else
        return oldText

{-# NOINLINE ref #-}
ref :: IORef State
ref = unsafePerformIO $ do
    now <- getTime
    newIORef (State now (now - 0) (pVerbatim ""))

data State = State {
    oldMeasureTime :: CpFloat, -- (POSIX) time of last measurement
    oldDiff :: CpFloat, -- old difference between POSIX time and space time of the physics engine
    oldText :: Prose
  }
