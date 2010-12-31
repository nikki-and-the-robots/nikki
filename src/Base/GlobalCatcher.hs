{-# language ViewPatterns #-}

module Base.GlobalCatcher where


import Prelude hiding (catch)

import Control.Exception

import System.Exit

import Graphics.Qt


globalCatcher :: IO () -> IO ()
globalCatcher cmd = flip catch catchAll cmd
  where
    catchAll :: SomeException -> IO ()
    catchAll e =
        if fromException e == Just ExitSuccess
        then return ()
        else do
            putStrLn ("error message:\n\n" ++ show e ++ "\n")
            exitWith (ExitFailure 1)
