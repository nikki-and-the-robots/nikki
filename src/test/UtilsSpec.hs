{-# language ViewPatterns #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module is both the place for testing the Utils module
-- and for providing utility functions for the testsuite
-- (which is admittedly unelegant)

module UtilsSpec where

import           Data.List
import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck

import           Graphics.Qt
import           Utils

spec :: Spec
spec = do
  describe "testMergePairs" $ do
    let testPredicate :: Int -> Int -> Maybe [Int]
        testPredicate a _ | a == 0 = Nothing
        testPredicate a b = if b `mod` a == 0 then Just [a] else Nothing

    it "is idempotent" $ do
      property $ \ l -> isIdempotent l (mergePairs testPredicate)

    it "reversal" $ do
      property $ \ (map abs -> l) ->
        null (mergePairs testPredicate l \\ mergePairs testPredicate (reverse l))

    it "done all" $ do
      property $ \ l -> all isNothing $ map (uncurry testPredicate) $
        completeEdges $ mergePairs testPredicate l

  describe "duplicates" $ do
    it "the correspondence between duplicates and nub" $ do
      property $ \ (list :: [Int]) ->
        (nub list == list) == null (duplicates list)

    it "tests that every duplicate exists more than once in a list" $ do
      property $ \ (list :: [Int]) ->
        all (\ dup -> length (filter (== dup) list) > 1) (duplicates list)

    it "tests that every non-duplicate exists exactly once" $ do
      property $ \ (list :: [Int]) -> all (\ dup -> length (filter (== dup) list) == 1)
        (filter (\ e -> not (e `elem` duplicates list)) list)

-- * these are test Utils (not tests for the Utils module)

isIdempotent :: Eq a => a -> (a -> a) -> Bool
isIdempotent x f =
    fx == f fx
  where
    fx = f x

-- | executes a test only once
quickCheckOnce :: Testable p => p -> IO ()
quickCheckOnce = quickCheckWith stdArgs{maxSuccess = 1}

testEquals :: (Show e, Eq e) => e -> e -> Property
testEquals a b =
    counterexample (show a ++ " /= " ++ show b) $
    a == b

(?=) :: (Show e, Eq e) => e -> e -> Property
(?=) = testEquals

(?~=) :: (Show e, Ord e, Fractional e) => e -> e -> Property
a ?~= b =
    counterexample (show a ++ " /~= " ++ show b) $
    a ~= b

unFixed :: Fixed a -> a
unFixed (Fixed a) = a

-- * some Arbitrary instances

instance Arbitrary a => Arbitrary (Position a) where
    arbitrary = Position <$> arbitrary <*> arbitrary
    shrink (Position x y) =
        [Position x' y | x' <- shrink x] ++
        [Position x y' | y' <- shrink y]
