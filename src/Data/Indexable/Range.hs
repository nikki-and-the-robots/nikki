{-# language DeriveDataTypeable #-}

module Data.Indexable.Range (
    Range,
    calculateRange,
    fmapMWithIndex,
  ) where


import Data.Data
import qualified Data.Vector as V

import Utils

import Data.Indexable (Indexable(..), Index(..))


data Range = SimpleRange Int Int
  deriving (Show, Typeable, Data)

-- | calculate the smallest range that includes all elements
-- satisfying the given condition.
calculateRange :: (a -> Bool) -> Indexable a -> Range
calculateRange p ix =
    SimpleRange (lowerBound elements) upperBound
  where
    lowerBound elements = length $ takeWhile (not . p) elements
    upperBound = (pred (length elements)) - (lowerBound (reverse elements))
    elements = ftoList ix


{-# inline fmapMWithIndex #-}
fmapMWithIndex :: (Monad m, Functor m) => Range -> (Index -> a -> m a)
    -> Indexable a -> m (Indexable a)
fmapMWithIndex (SimpleRange lower upper) cmd (Indexable values) = do
    let (prefix, rest) = V.splitAt lower values
        (unprocessedMiddle, suffix) = V.splitAt (upper - lower) rest
    processedMiddle <- fmapM (\ (i, v) -> tuple i <$> cmd i v) unprocessedMiddle
    return $ Indexable (prefix V.++ processedMiddle V.++ suffix)
