

module Main where


import Prelude hiding (log)

import System.Environment
import System.Process
import System.Exit

import Distribution.AutoUpdate.Paths


main = do
    executable <- findCoreExecutable
    args <- getArgs
    loop 5 $ rawSystem executable args
  where
    loop 0 _ = error "restarted 5 times: aborting"
    loop n action = do
        exitCode <- action
        case exitCode of
            ExitFailure 143 -> do
                loop (n - 1) action
            _ -> do
                exitWith exitCode
