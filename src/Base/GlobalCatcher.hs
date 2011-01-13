{-# language ViewPatterns #-}

module Base.GlobalCatcher (
    forkLogicThread,
  ) where


import Prelude hiding (catch)

import Text.Logging

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
            logInfo "caught"
            handleException e
            let exitCode = case fromException e of
                    (Just x) -> x
                    Nothing -> ExitFailure 1
            putMVar exitCodeMVar exitCode
            throw e
    forkOS $ catch (action >> putMVar exitCodeMVar ExitSuccess) catchAll
    return exitCodeMVar

-- | standard handling of exceptions
handleException :: SomeException -> IO ()
handleException e =
    case fromException e of
        (Just ExitSuccess) -> return ()
        _ -> do
            let msg = "error message:\n\n" ++ show e ++ "\n"
            logInfo msg
