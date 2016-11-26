{-# language ScopedTypeVariables, DeriveDataTypeable #-}

module Top.MainSpec where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Data.Typeable
import           Prelude hiding (catch)
import           System.Exit
import           Test.Hspec
import           Test.QuickCheck.Property

import           Top.Main
import           Utils.Tests

data E = E String
  deriving (Show, Eq, Typeable)

instance Exception E

spec :: Spec
spec = do
  describe "forkThreads" $ do
    it "re-throws exceptions from the logic thread" $ do
      let render mvar = do
            putMVar mvar ()
            wait 1
          logic () = do
            wait 0.5
            throw $ E "exception"
      forkThreads render logic `shouldThrow` (== (E "exception"))

    it "relays exitWith calls from the logic thread" $ do
      let render mvar = do
            putMVar mvar ()
            wait 1
          logic () = do
            wait 0.5
            exitWith $ ExitFailure 42
      forkThreads render logic `shouldThrow` (== (ExitFailure 42))

wait seconds =
    threadDelay $ round (seconds * 10 ^ 6)
