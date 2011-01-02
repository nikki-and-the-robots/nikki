{-# language ViewPatterns #-}

module Base.GlobalCatcher where


import Prelude hiding (catch)

import Control.Exception
import Control.Concurrent

import System.Exit


globalCatcher :: ThreadId -> IO () -> IO ()
globalCatcher mainThread cmd = flip catch catchAll cmd
  where
    catchAll :: SomeException -> IO ()
    catchAll e =
        case fromException e of
            (Just ExitSuccess) -> return ()
            (Just failure) -> throwTo mainThread e
            Nothing -> do
                putStrLn ("error message:\n\n" ++ show e ++ "\n")
                throwTo mainThread e
