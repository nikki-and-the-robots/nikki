
-- | implements some missing functionality for System.Console.CmdArgs


module Distribution.AutoUpdate.CmdArgs where


import System.IO
import System.Exit
import System.Environment
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit


cmdTheseArgs :: Data a => a -> [String] -> IO a
cmdTheseArgs options args =
    case process (cmdArgsMode options) args of
        Left x -> hPutStrLn stderr x >> exitFailure
        Right cmdArgs -> cmdArgsApply cmdArgs
