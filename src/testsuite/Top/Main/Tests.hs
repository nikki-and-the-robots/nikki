{-# language ScopedTypeVariables, DeriveDataTypeable #-}

module Top.Main.Tests where


import Prelude hiding (catch)

import Data.Typeable

import Control.Applicative
import Control.Concurrent
import Control.Exception

import System.Exit

import Top.Main


import Test.QuickCheck.Property

import Utils.Tests



tests = do
    quickCheckOnce exceptionByLogicThread
    quickCheckOnce exitCodeByLogicThread


exceptionByLogicThread = assertException (E "exception") $ do
    forkThreads render logic
    return ()
  where
    render mvar = do
        putMVar mvar ()
        wait 1

    logic () = do
        wait 0.5
        throw $ E "exception"

data E = E String
  deriving (Show, Eq, Typeable)
instance Exception E

exitCodeByLogicThread = assertException (ExitFailure 42) $ do
    forkThreads render logic
    return ()
  where
    render mvar = do
        putMVar mvar ()
        wait 1

    logic () = do
        wait 0.5
        exitWith $ ExitFailure 42


-- * util

wait seconds =
    threadDelay $ round (seconds * 10 ^ 6)

assertException :: forall a . (Exception a, Eq a) => a -> IO () -> Property
assertException e cmd = morallyDubiousIOProperty $ do
    (cmd *> notThrown) `catch` handle
  where
    notThrown = return $ printTestCase ("not thrown: " ++ show e) False

    handle :: a -> IO Property
    handle thrown = return $
        printTestCase (show e ++ "\n/=\n" ++ show thrown) $
        (thrown == e)

