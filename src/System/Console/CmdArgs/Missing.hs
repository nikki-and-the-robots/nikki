
-- | Implements functionality for System.Console.CmdArgs.
-- You could do the same with 'withArgs', but that conflicts
-- with FindBin (ghc internals).


module System.Console.CmdArgs.Missing where


import System.IO
import System.Exit
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit


cmdTheseArgs :: Data a => a -> [String] -> IO a
cmdTheseArgs options args =
    case process (cmdArgsMode options) args of
        Left x -> hPutStrLn stderr x >> exitFailure
        Right cmdArgs -> cmdArgsApply cmdArgs
