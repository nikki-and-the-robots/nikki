

module Main where


import Text.Logging

import Control.Monad

import System.Environment
import System.Process
import System.Exit

import Distribution.AutoUpdate.Paths

import Base.Configuration


main = do
    loadConfiguration
    executable <- findCoreExecutable
    args <- getArgs
    loop 5 $ rawSystem executable args
  where
    loop 0 action = error "restarted 5 times: aborting"
    loop n action = do
        logInfo "starting core..."
        exitCode <- action
        case exitCode of
            ExitFailure 143 -> do
                logInfo (show exitCode ++ " -> restarting")
                loop (n - 1) action
            _ -> do
                logInfo (show exitCode ++ " -> not restarting")
                exitWith exitCode
