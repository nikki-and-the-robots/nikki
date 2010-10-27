
-- | module for clocking operations.
-- If you want an operation 'op' to be done every second, you could do:
--
-- main = do
--     timer <- newTickTimer 1.0
--     loop timer
-- loop timer = do
--     op
--     waitTick timer
--     loop
--
-- waitTick will try to wait for some time so that
--  1. op will be executed once per second, if it takes lesser than 1 second (minus epsilon)
--  2. op will never be executed more than once per second.
--     So, if op once needs more time than 1 second, waitTick won't try to catch up the lost time.

module Control.Concurrent.TickTimer (TickTimer, newTickTimer, waitTick) where


import Data.IORef
import Data.Time.Clock.POSIX

import Control.Applicative
import Control.Monad
import Control.Concurrent


data TickTimer = TickTimer Double (IORef Double)

newTickTimer :: Double -> IO TickTimer
newTickTimer secondsToPass =
    TickTimer secondsToPass <$> (newIORef =<< getMyTime)

getMyTime = realToFrac <$> getPOSIXTime

waitTick :: TickTimer -> IO ()
waitTick (TickTimer secondsToPass oldRef) = do
    oldNow <- readIORef oldRef
    now <- getMyTime
    let waitTime = (secondsToPass - (now - oldNow)) - (0.055 * 0.002)
    when (waitTime > 0) $ do
        threadDelay $ round (waitTime * 10 ^ 6)
    now <- getMyTime
    writeIORef oldRef now
