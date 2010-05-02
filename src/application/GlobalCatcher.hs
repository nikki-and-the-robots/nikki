{-# language ViewPatterns #-}

module GlobalCatcher where


import Control.Exception as E

import System.Exit


globalCatcherGame :: IO () -> IO ()
globalCatcherGame = globalCatcherEditor

globalCatcherEditor :: IO () -> IO ()
globalCatcherEditor cmd = flip E.catch handler cmd
  where
    handler :: SomeException -> IO ()
    handler (show -> "ExitSuccess") = return ()
    handler e = do
        globalCatcherEditor $ mapM_ putStrLn (text e)
        exitWith (ExitFailure 42)
    text e = [
            "error message:",
            "",
            show e
          ]


