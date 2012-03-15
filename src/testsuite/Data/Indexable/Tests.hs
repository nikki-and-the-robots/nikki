{-# language ScopedTypeVariables #-}

module Data.Indexable.Tests where


import Prelude hiding (foldr)

import Data.Indexable hiding (length, filter)
import qualified Data.Indexable as I
import Data.List (sort, nub)
import Data.Foldable (foldr)
import qualified Data.Vector as Vector

import Control.Monad.State
import Control.Arrow
import Control.DeepSeq

import Utils hiding ((==>))

import Test.QuickCheck


tests :: IO ()
tests = do
    quickCheck testLength
    quickCheck testILength
    quickCheck listEquality
    quickCheck indexing
    quickCheck indexPreservingSort
    quickCheck fmapTest
    quickCheck fmapM_Test
    quickCheck fmapMTest
    quickCheck foldTest
    quickCheck deleteTest
    quickCheck traverseTest
    quickCheck insertion
    quickCheck mergeIdempotency
    quickCheck mergeTest
    quickCheck indexAGetter
    quickCheck indexASetter
    quickCheck toHeadTest
    quickCheck toLastTest
    quickCheck showTestEquality
    return ()

instance (Arbitrary a) => Arbitrary (Indexable a) where
    arbitrary = do
        values <- arbitrary
        keys <-
            take (length values) <$>
            randomPermutation (map Index [0 .. (length values * 5)])
        error $ show $ sort keys
        return $ Indexable $ Vector.fromList $ zip keys values
    shrink =
        values >>> Vector.toList >>>
        map (first (index >>> Fixed)) >>>
        shrink >>>
        map (map (first (fromIntegral >>> Index))) >>>
        map (Indexable . Vector.fromList)

randomPermutation :: [a] -> Gen [a]
randomPermutation [] = return []
randomPermutation list = do
    i <- choose (0, length list - 1)
    let a = list !! i
    rest <- randomPermutation (take i list ++ drop (i + 1) list)
    return (a : rest)


-- * tests

testLength :: [Double] -> Bool
testLength l = length l == I.length (fromList l)

testILength :: Indexable Double -> Bool
testILength ix =
    (I.length ix == length (toList ix)) &&
    (I.length ix == Vector.length (values ix))

listEquality :: [Double] -> Bool
listEquality x = x == toList (fromList x)


indexing :: [Double] -> Property
indexing list =
    not (null list) ==>
    forAll (choose (0, length list - 1)) $ \ i ->
    list !! i == fromList list !!! Index i

-- | tests if the indices are preserved during sorting
indexPreservingSort :: Indexable Double -> Bool
indexPreservingSort ix =
    sort (keys ix) == sort (keys sorted) &&
    map (ix !!!) (keys ix) == map (sorted !!!) (keys ix) &&
    sort sortedToList == sortedToList
  where
    sorted = sortBy compare ix
    sortedToList = toList sorted

fmapTest :: Double -> Double -> Indexable Double -> Bool
fmapTest m c ix =
    sort (keys ix) == sort (keys mapped) &&
    all (\ i -> (fun (ix !!! i)) == mapped !!! i) (keys ix)
  where
    mapped = fmap fun ix
    fun x = x * m + c

fmapM_Test :: Bool
fmapM_Test =
    let ix :: Indexable Int = Indexable $ Vector.fromList $ Prelude.zip [0, 2 ..] [0..300]
        r :: Maybe () = fmapM_ (\ x -> deepseq x `seq` return ()) ix
    in r `seq` True

fmapMTest :: Double -> Double -> Indexable Double -> Bool
fmapMTest m c ix = 
    case monad of
        Just True -> True
        _ -> False
  where
    monad = do
        mapped <- fmapM action ix
        let keysEqual = sort (keys ix) == sort (keys mapped)
        equals <- forM (keys ix) $ \ i -> do
            x <- action (ix !!! i)
            return (x == mapped !!! i)
        return (keysEqual && all id equals)
    action x = return (x * m + c)

foldTest :: [Double] -> Bool
foldTest list =
    list == foldr (:) [] (fromList list)

deleteTest :: Indexable Double -> Property
deleteTest ix =
    printTestCase "deleteByIndex" $
    not (null (keys ix)) ==>
    forAll (elements (keys ix)) $ \ i ->
    not (i `isIndexOf` (deleteByIndex i ix))

traverseTest :: Indexable Double -> Property
traverseTest ix =
    printTestCase "traverse" $
    expected == traverseList
  where
    expected = toList ix
    traverseList :: [Double]
    traverseList = execState (fmapM_ acc ix) []
    acc :: Double -> State [Double] ()
    acc d = modify (+: d)

insertion :: Indexable Double -> Double -> Double -> Bool
insertion ix a b =
    a == head (toList (a <: ix)) &&
    b == last (toList (ix >: b))

mergeIdempotency :: Indexable [Int] -> Bool
mergeIdempotency ix =
    merged == merge merged
  where
    merged = merge ix
    merge = optimizeMerge mergeFunction

mergeFunction :: [Int] -> [Int] -> Maybe [Int]
mergeFunction a b | all odd a && all odd b = Just $ nub (a ++ b)
mergeFunction _ _ = Nothing

mergeTest :: Indexable [Int] -> Property
mergeTest ix =
    printTestCase "mergeTest"
    (length (filter (all odd) mergedList) <= 1 &&
    filter (not . all odd) mergedList == filter (not . all odd) originalList)
  where
    mergedList = toList $ optimizeMerge mergeFunction ix
    originalList = toList ix

indexAGetter :: Indexable Double -> Property
indexAGetter ix =
    not (null $ toList ix) ==>
    forAll (elements $ keys ix) $ \ i ->
        (ix ^. indexA i == ix !!! i)

indexASetter :: Indexable Double -> Double -> Property
indexASetter ix e =
    not (null $ toList ix) ==>
    forAll (elements $ keys ix) $ \ i ->
        (((indexA i ^= e) ix) !!! i == e)

toHeadTest :: Indexable Double -> Property
toHeadTest = toEndTest toHead head

toLastTest :: Indexable Double -> Property
toLastTest = toEndTest toLast last

-- | generelizes toHeadTest and toLastTest
toEndTest :: (Index -> Indexable Double -> Indexable Double) -> ([Double] -> Double)
    -> Indexable Double -> Property
toEndTest toEnd listSelector ix =
    not (null $ toList ix) ==>
    forAll (elements $ keys ix) $ \ i ->
        ((ix !!! i) == (listSelector $ toList $ toEnd i ix))

-- | tests if (show . read) == id
showTestEquality :: Indexable Double -> Bool
showTestEquality ix = ix == (read $ show ix)
