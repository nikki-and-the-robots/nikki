{-# language ViewPatterns #-}

module Base.GlobalCatcher where


import Control.Exception as E

import System.Exit


globalCatcher :: IO () -> IO ()
globalCatcher cmd = flip E.catch handler cmd
  where
    handler :: SomeException -> IO ()
    handler (show -> "ExitSuccess") = return ()
    handler e = do
        globalCatcher $ mapM_ putStrLn (text e)
        exitWith (ExitFailure 42)
    text e = [
            "error message:",
            "",
            show e
          ]


