

module Main where


import Prelude hiding (log)

import System.Environment
import System.Process
import System.Exit
import System.Info

import Distribution.AutoUpdate.Paths


main = do
    executable <- findCoreExecutable
    args <- getArgs
    loop 5 $ rawSystem executable args
  where
    loop 0 action = error "restarted 5 times: aborting"
    loop n action = do
        log "starting core..."
        exitCode <- action
        case exitCode of
            ExitFailure 143 -> do
                log (show exitCode ++ " -> restarting")
                loop (n - 1) action
            _ -> do
                log (show exitCode ++ " -> not restarting")
                exitWith exitCode

log = case System.Info.os of
    "mingw32" -> appendFile "nikkiLog" . (++ "\n")
    _ -> putStrLn

