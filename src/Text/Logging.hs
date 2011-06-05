
-- | Module for logging.

module Text.Logging (LogLevel(..), logg) where


import System.Info
import System.FilePath
import System.Environment
import System.Environment.FindBin


data LogLevel
    = Debug | Info | Warning | Error
  deriving Show

-- | Logs a message with the given log level.
-- Prints to stdout on unix, uses a logFile on windows.
logg :: LogLevel -> String -> IO ()
logg ll msg =
    inner $ mkMsg ll msg
  where
    inner = case System.Info.os of
        "mingw32" -> windowsLogging
        _ -> putStrLn

mkMsg :: LogLevel -> String -> String
mkMsg ll msg =
    show ll ++ ": " ++ msg

windowsLogging :: String -> IO ()
windowsLogging msg = do
    progPath <- getProgPath
    progName <- getProgName
    appendFile (progPath </> progName <.> "log") msg
