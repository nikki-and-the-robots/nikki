{-# language ViewPatterns #-}

module Base.GlobalCatcher (
    forkLogicThread,
  ) where


import Prelude hiding (catch)

import Control.Exception
import Control.Concurrent

import System.IO
import System.Exit

import Utils


-- | Runs the given action. Catches any exception and saves them
-- in the returned MVar.
forkLogicThread :: IO () -> IO (MVar ExitCode)
forkLogicThread action = do
    exitCodeMVar <- newEmptyMVar
    let catchAll :: SomeException -> IO ()
        catchAll e = do
            putStrLn "caught"
            handleException e
            let exitCode = case fromException e of
                    (Just x) -> x
                    Nothing -> ExitFailure 1
            putMVar exitCodeMVar exitCode
            throw e
    forkOS $ catch action catchAll
    return exitCodeMVar

-- | standard handling of exceptions
handleException e = do
    let msg = "error message:\n\n" ++ show e ++ "\n"
    putStrLn msg
