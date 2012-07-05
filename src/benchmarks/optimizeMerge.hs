

import Data.Indexable as Ix

import Control.DeepSeq
import Control.Applicative
import Control.Exception

import System.Random

import Criterion.Main

main :: IO ()
main = do
    ix <- testIndexable
    -- ~ deepseq ix (return ())
    test ix
    
    -- This does not terminate, but why?
    -- ~ defaultMain $ 
        -- ~ (bench "optimizeMerge" $ print 42) :
        -- ~ []

testIndexable :: IO (Indexable Int)
testIndexable = do
    Ix.fromList <$> mapM (const $ randomRIO (0, 1000)) [1 .. len]
  where
    len = 1000

mergeInts :: Int -> Int -> Maybe Int
mergeInts a b = if even a && even b then Just (a + b) else Nothing

-- | very simple tests
test :: Indexable Int -> IO ()
test ix = do
    let merged = optimizeMerge mergeInts ix
    assert (pLength odd ix == pLength odd merged) (return ())
    print (Ix.toList merged)
    assert (pLength even merged <= 1) (return ())
  where
    -- count the elements where (p element) == True
    pLength :: (a -> Bool) -> Indexable a -> Int
    pLength p ix = Ix.length $ Ix.filter p ix
