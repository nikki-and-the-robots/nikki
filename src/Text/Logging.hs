
-- | Module for logging.

module Text.Logging (
    LogLevel(..),
    logg,
    loggUnsafe,
    setLogFile,
  ) where


import Data.IORef

import Control.Monad
import Control.Monad.IO.Class

import System.Info
import System.FilePath
import System.Environment
import System.Environment.FindBin
import System.IO
import System.IO.Unsafe


data LogLevel
    = Debug | Info | Warning | Error
  deriving (Eq, Ord, Show)

printLogLevel = Warning

-- | Logs a message with the given log level.
-- Prints to stdout on unix, uses a logFile on windows.
logg :: (MonadIO m) => LogLevel -> String -> m ()
logg ll msg =
    liftIO $ when (ll >= printLogLevel) $
    inner $ mkMsg ll msg
  where
    inner msg = do
        mLogHandle <- readIORef _logHandle
        case mLogHandle of
            Just logHandle -> hPutStrLn logHandle msg >> hFlush logHandle
            Nothing -> case System.Info.os of
                "mingw32" -> windowsLogging msg
                _ -> putStrLn msg >> hFlush stdout

mkMsg :: LogLevel -> String -> String
mkMsg ll msg =
    show ll ++ ": " ++ msg

windowsLogging :: String -> IO ()
windowsLogging msg = do
    progPath <- getProgPath
    progName <- getProgName
    appendFile (progPath </> progName <.> "log") (msg ++ "\n")


loggUnsafe :: LogLevel -> String -> a -> a
loggUnsafe ll msg a = unsafePerformIO $ do
    logg ll msg
    return a


-- * log file

{-# noinline _logHandle #-}
_logHandle :: IORef (Maybe Handle)
_logHandle = unsafePerformIO $ newIORef Nothing

setLogFile :: FilePath -> IO ()
setLogFile logFile =
    openFile logFile AppendMode >>=
    writeIORef _logHandle . Just
