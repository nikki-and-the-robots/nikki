{-# language ScopedTypeVariables, DeriveDataTypeable #-}

module Top.Main.Tests where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Data.Typeable
import           Prelude hiding (catch)
import           System.Exit
import           Test.QuickCheck.Property

import           Top.Main
import           Utils.Tests

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
assertException e cmd = ioProperty $ do
    (cmd *> notThrown) `catch` handle
  where
    notThrown = return $ counterexample ("not thrown: " ++ show e) False

    handle :: a -> IO Property
    handle thrown = return $
        counterexample (show e ++ "\n/=\n" ++ show thrown) $
        (thrown == e)

