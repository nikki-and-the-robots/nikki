
-- | Module for logging:
-- Decides how to log.
-- Saves a global logging operation.

module Text.Logging (logInfo, setLogCommand, printInfo) where


import Control.Monad.IO.Class
import Control.Concurrent.MVar

import System.IO.Unsafe

-- | log something (info level)
logInfo :: MonadIO m => String -> m ()
logInfo msg = liftIO $ do
    logCommand <- readMVar logCommandRef
    logCommand msg

-- | like print, but uses the logging mechanisms of this module
-- (instead of stdout)
printInfo :: (MonadIO m, Show s) => s -> m ()
printInfo x = logInfo (show x)

{-# NOINLINE logCommandRef #-}
logCommandRef :: MVar (String -> IO ())
logCommandRef = unsafePerformIO $ newMVar putStrLn

-- | sets a new log command.
setLogCommand :: MonadIO m => (String -> IO ()) -> m ()
setLogCommand logCommand = liftIO $ do
    modifyMVar_ logCommandRef (const $ return logCommand)
    return ()
