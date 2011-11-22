{-# language ViewPatterns #-}
-- | This module is both the place for testing the Utils module
-- and for providing utility functions for the testsuite
-- (which is admittedly unelegant)

module Utils.Tests where


import Data.List
import Data.Maybe

import Graphics.Qt

import Utils

import Test.QuickCheck


tests :: IO ()
tests = do
    testMergePairs
    testDuplicates

testMergePairs :: IO ()
testMergePairs = mapM_ (quickCheckWith stdArgs{maxSuccess = 1000}) [
    putTestCase "testMergePairs.isIdempotent" $ \ l -> isIdempotent l (mergePairs testPredicate),
    putTestCase "testMergePairs.reversal" $ \ (map abs -> l) ->
        null (mergePairs testPredicate l \\ mergePairs testPredicate (reverse l)),
    putTestCase "testMergePairs.done all" $ \ l -> all isNothing $ map (uncurry testPredicate) $
        completeEdges $ mergePairs testPredicate l
   ]

testPredicate :: Int -> Int -> Maybe [Int]
testPredicate a b | a == 0 = Nothing
testPredicate a b = if b `mod` a == 0 then Just [a] else Nothing

testDuplicates = do
    -- tests the correspondence between duplicates and nub
    quickCheckIntList (\ list -> ((nub list == list) == null (duplicates list)))
    -- tests that every duplicate exists more than once in a list
    quickCheckIntList (\ list -> all (\ dup -> length (filter (== dup) list) > 1) (duplicates list))
    -- tests that every non-duplicate exists exactly once
    quickCheckIntList (\ list -> all (\ dup -> length (filter (== dup) list) == 1)
        (filter (\ e -> not (e `elem` duplicates list)) list))
  where
    quickCheckIntList :: Testable p => ([Int] -> p) -> IO ()
    quickCheckIntList = quickCheck


-- * these are test Utils (not tests for the Utils module)

isIdempotent :: Eq a => a -> (a -> a) -> Bool
isIdempotent x f =
    fx == f fx
  where
    fx = f x

-- | attaches a message to a property that will be printed in case of failure
putTestCase :: Testable p => String -> p -> Property
putTestCase msg p = whenFail (putStrLn msg) p

-- | executes a test only once
quickCheckOnce :: Testable p => p -> IO ()
quickCheckOnce = quickCheckWith stdArgs{maxSuccess = 1}

-- | tests a list of possible offending values
testExamples :: Testable p => String -> (a -> p) -> [a] -> IO ()
testExamples msg p examples =
    mapM_ (\ (i, example) -> quickCheckOnce $
        putTestCase (msg ++ " element no.: " ++ show i) $
        p example)
        (zip [0..] examples)

testEquals :: (Show e, Eq e) => e -> e -> Property
testEquals a b =
    printTestCase (show a ++ " /= " ++ show b) $
    a == b

(?=) :: (Show e, Eq e) => e -> e -> Property
(?=) = testEquals

(?~=) :: (Show e, Ord e, Fractional e) => e -> e -> Property
a ?~= b =
    printTestCase (show a ++ " /~= " ++ show b) $
    a ~= b

unFixed :: Fixed a -> a
unFixed (Fixed a) = a

-- * some Arbitrary instances

instance Arbitrary a => Arbitrary (Position a) where
    arbitrary = Position <$> arbitrary <*> arbitrary
    shrink (Position x y) =
        [Position x' y | x' <- shrink x] ++
        [Position x y' | y' <- shrink y]
