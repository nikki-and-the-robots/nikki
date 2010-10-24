{-# language ViewPatterns #-}

module Base.GlobalCatcher where


import Control.Exception as E

import System.Exit

import Graphics.Qt


globalCatcher :: IO () -> IO ()
globalCatcher cmd = flip E.catch handler cmd
  where
    handler :: SomeException -> IO ()
    handler (show -> "ExitSuccess") = return ()
    handler e = do
        putStrLn ("error message:\n\n" ++ show e ++ "\n")
        quitQApplication
        exitWith (ExitFailure 42)


