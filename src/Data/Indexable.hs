{-# language GeneralizedNewtypeDeriving, DeriveDataTypeable, NamedFieldPuns,
     ViewPatterns #-}

-- | module for a Bag of indexed things. 
-- They have an order (can be converted to a list.)
-- imports could look like this:
--
-- import qualified Data.Indexable as I
-- import Data.Indexable hiding (length, toList, findIndices, fromList, empty)

module Data.Indexable (
    Indexable(..),
    Index(..),

    length,
    toList,
    (!!!),
    findIndices,
    filter,
    sortBy,

    fromList,
    (<:),
    (>:),

    fmapMWithIndex,

    deleteByIndex,
    indexA,
    toHead,
    toLast,
    isIndexOf,

    optimizeMerge,
  ) where

import Prelude hiding (length, filter)

import qualified Data.IntMap as Map
import Data.IntMap ((!))
import qualified Data.List as List
import Data.Generics
import Data.Either
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid
import Data.Initial
import Data.Accessor

import Control.Applicative (Applicative, pure)
import Control.Arrow

import Utils


newtype Index = Index {index :: Int}
  deriving (Show, Read, Enum, Num, Eq, Integral, Real, Ord, Data, Typeable)


-- | invariants:
-- sort (keys x) == sort (Map.keys (values x))
-- nub (keys x) == keys x
-- const True (keys x == sort (keys x))  (that is, the keys may be unsorted)
data Indexable a = Indexable {
    values :: Map.IntMap a,
    keys :: [Index]
  }
    deriving (Show, Read, Data, Typeable)


-- * instances

instance Functor Indexable where
    fmap f (Indexable values keys) = Indexable (fmap f values) keys

instance Foldable Indexable where
    foldMap f (Indexable values keys) =
        foldMap (\ k -> f (values ! index k)) keys

instance Traversable Indexable where
    traverse cmd (Indexable values keys) =
        Indexable <$> traverse cmd values <*> pure keys

fmapMWithIndex :: Monad m => (Index -> a -> m b)
    -> Indexable a -> m (Indexable b)
fmapMWithIndex cmd (Indexable values keys) = do
    newValues <- mapM (\ k -> cmd k (values ! (index k))) keys
    return $ Indexable (Map.fromList $ zip (map index keys) newValues) keys

instance Initial (Indexable a) where
    initial = Indexable initial initial

-- * getter

-- | returns the length of the contained list
length :: Indexable a -> Int
length = List.length . keys

-- -- | returns, if the Index points to something
isIndexOf :: Index -> Indexable a -> Bool
isIndexOf i indexable = i `elem` keys indexable

toList :: Indexable a -> [a]
toList x = map (((values x) !) . index) $ keys x

(!!!) :: Indexable a -> Index -> a
Indexable{values} !!! i =
    case Map.lookup (index i) values of
        Just x -> x
        Nothing -> error ("!!!: Index not found")

-- | returns the list if indices for which the corresponding
-- values fullfill a given predicate.
-- Honours the order of values.
findIndices :: (a -> Bool) -> Indexable a -> [Index]
findIndices p (Indexable values keys) =
    List.filter (p . (values !) . index) keys

filter :: (a -> Bool) -> Indexable a -> Indexable a
filter p ixs = Indexable newValues newIndices
  where
    newIndices = findIndices p ixs
    newValues = Map.fromList $ map (\ i -> (index i, ixs !!! i)) newIndices

-- | Stable sorting of Indexables while preserving indices.
sortBy :: (a -> a -> Ordering) -> Indexable a -> Indexable a
sortBy ordering (Indexable values keys) =
    Indexable values (map (Index . fst) sortedPairList)
  where
--     sortedPairList :: [(Int, a)]
    sortedPairList = List.sortBy (withView snd ordering) pairList
--     pairList :: [(Int, a)]
    pairList = map (\ i -> (i, values ! i)) $ map index keys

-- | generate an unused Index
-- (newIndex l) `elem` l == False
newIndex :: [Index] -> Index
newIndex [] = 0
newIndex l = maximum l + 1

-- * constructors

(<:) :: a -> Indexable a -> Indexable a
a <: (Indexable values keys) =
    Indexable (Map.insert (index i) a values) (i : keys)
  where
    i = newIndex keys

(>:) :: Indexable a -> a -> Indexable a
(Indexable values keys) >: a =
    Indexable (Map.insert (index i) a values) (keys +: i)
  where
    i = newIndex keys

fromList :: [a] -> Indexable a
fromList list =
    Indexable (Map.fromList pairs) (map (Index . fst) pairs)
  where
    pairs = zip [0..] list

-- * mods

deleteByIndex :: Index -> Indexable a -> Indexable a
deleteByIndex i (Indexable values keys) =
    Indexable (Map.delete (index i) values) (List.filter (/= i) keys)

indexA :: Index -> Accessor (Indexable a) a
indexA i = accessor getter setter
  where
    getter (Indexable values keys) | i `elem` keys =
        values ! index i
    setter e (Indexable values keys) | i `elem` keys =
        Indexable (Map.insert (index i) e values) keys

-- | puts the indexed element first
toHead :: Index -> Indexable a -> Indexable a
toHead i (Indexable values keys) | i `elem` keys =
    Indexable values (i : List.filter (/= i) keys)

-- | puts the indexed element last
toLast :: Index -> Indexable a -> Indexable a 
toLast i (Indexable values keys) | i `elem` keys =
    Indexable values (List.filter (/= i) keys +: i)


-- | optimizes an Indexable with merging.
-- calls the given function for every pair in the Indexable.
-- the given function returns Nothing, if nothing can be optimized and
-- returns the replacement for the optimized pair.
-- The old pair will be replaced with dummy elements.
-- This function is idempotent. (if that's an english word)
-- Note, that indices of optimized items are going to be invalidated.
optimizeMerge :: Show a => (a -> a -> Maybe a) -> Indexable a -> Indexable a
optimizeMerge p =
    convertToList >>> fixpoint 0 >>> convertToIndexable
  where
    fixpoint n list =
--         trace (show n) $
        let r = mergeListSome p list
        in if List.length r == List.length list then 
            list
          else
            fixpoint (n + 1) r


    convertToList :: Indexable a -> [Either (Index, a) a] -- left unmerged, right merged
    convertToList ix = map (\ i -> Left (i, values ix ! index i)) (keys ix)
    convertToIndexable :: [Either (Index, a) a] -> Indexable a
    convertToIndexable list =
        Indexable (Map.fromList (map (first index) newValues)) (map fst newValues)
      where
        newValues = zipWith inner list newIndices
        newIndices = [maximum allIndices + 1..]
        allIndices = map fst $ lefts list
        inner (Left x) _ = x
        inner (Right x) i = (i, x)

mergeListSome :: (a -> a -> Maybe a) -> [(Either (Index, a) a)] -> [(Either (Index, a) a)]
mergeListSome p (a : r) =
        case mergeSome p a ([], r) of
            Just (merged, r') -> Right merged : mergeListSome p r'
            Nothing -> a : mergeListSome p r
  where
    mergeSome :: (a -> a -> Maybe a)
        -> Either (Index, a) a
        -> ([(Either (Index, a) a)], [(Either (Index, a) a)])
        -> Maybe (a, [(Either (Index, a) a)])
    mergeSome p outerA (before, (outerB : r)) =
        case p (getInner outerA) (getInner outerB) of
            Just x -> Just (x, reverse before ++ r)
            Nothing -> mergeSome p outerA (outerB : before, r)
    mergeSome _ _ (_, []) = Nothing

    getInner (Left (_, a)) = a
    getInner (Right b) = b


mergeListSome _ [] = []
