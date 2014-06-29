
-- | Module for logging.

module Text.Logging (
    LogLevel(..),
    logg,
    loggUnsafe,
    setLogFile,
  ) where


import Data.IORef
import qualified Data.ByteString as SBS
import Data.String.Conversions

import Control.Monad
import Control.Monad.IO.Class

import System.Info
import System.FilePath
import System.Environment
import System.IO
import System.IO.Unsafe

import Utils.Scripting


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
            Just logHandle -> SBS.hPutStr logHandle msg >> hFlush logHandle
            Nothing -> case System.Info.os of
                "mingw32" -> windowsLogging msg
                _ -> SBS.putStr msg >> hFlush stdout

mkMsg :: LogLevel -> String -> SBS.ByteString
mkMsg ll msg =
   cs (show ll ++ ": " ++ msg ++ "\n")

windowsLogging :: SBS.ByteString -> IO ()
windowsLogging msg = do
    progPath <- getProgPathOrCurrentDirectory
    progName <- getProgName
    SBS.appendFile (progPath </> progName <.> "log") msg


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

