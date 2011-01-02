

module Main where


import Control.Monad

import System.Environment
import System.Process
import System.Exit

import Distribution.AutoUpdate.Paths


main = do
    executable <- findCoreExecutable
    args <- getArgs
    loop 5 $ rawSystem executable args
  where
    loop 0 action = error "restarted 5 times: aborting"
    loop n action = do
        exitCode <- action
        case exitCode of
            ExitFailure 143 -> do
                putStrLn (show exitCode ++ " -> restarting")
                loop (n - 1) action
            _ -> do
                putStrLn (show exitCode ++ " -> not restarting")
                exitWith exitCode
