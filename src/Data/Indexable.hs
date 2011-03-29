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
    keys,

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

import qualified Data.List as List
import Data.Generics
import Data.Either
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap, toList)
import Data.Initial
import Data.Accessor

import Control.Arrow

import Utils


newtype Index = Index {index :: Int}
  deriving (Show, Read, Enum, Num, Eq, Integral, Real, Ord, Data, Typeable)


-- | invariants:
-- sort (keys x) == sort (Map.keys (values x))
-- nub (keys x) == keys x
-- const True (keys x == sort (keys x))  (that is, the keys may be unsorted)
newtype Indexable a = Indexable {
    values :: [(Index, a)]
  }
    deriving (Show, Read, Data, Typeable, Eq)

keys :: Indexable a -> [Index]
keys = map fst . values


-- * instances

instance Functor Indexable where
    fmap f (Indexable values) = Indexable (fmap (second f) values)

instance Foldable Indexable where
    foldMap f (Indexable values) =
        foldMap (f . snd) values

instance Traversable Indexable where
    traverse cmd (Indexable values) =
        Indexable <$> traverse inner values
      where
        inner (k, v) = tuple k <$> cmd v

fmapMWithIndex :: (Monad m, Functor m) => (Index -> a -> m b)
    -> Indexable a -> m (Indexable b)
fmapMWithIndex cmd (Indexable values) = 
    Indexable <$> fmapM (\ (i, v) -> tuple i <$> cmd i v) values

instance Initial (Indexable a) where
    initial = Indexable initial

-- * getter

-- | returns the length of the contained list
length :: Indexable a -> Int
length = List.length . values

-- -- | returns, if the Index points to something
isIndexOf :: Index -> Indexable a -> Bool
isIndexOf i indexable = i `elem` keys indexable

(!!!) :: Indexable a -> Index -> a
(Indexable values) !!! i =
    case lookup i values of
        Just x -> x
        Nothing -> error ("!!!: Index not found")

-- | returns the list of indices for which the corresponding
-- values fullfill a given predicate.
-- Honours the order of values.
findIndices :: (a -> Bool) -> Indexable a -> [Index]
findIndices p (Indexable values) =
    map fst $ List.filter (p . snd) values

filter :: (a -> Bool) -> Indexable a -> Indexable a
filter p (Indexable values) =
    Indexable $ List.filter (p . snd) values

-- | Stable sorting of Indexables while preserving indices.
sortBy :: (a -> a -> Ordering) -> Indexable a -> Indexable a
sortBy ordering (Indexable values) =
    Indexable $ List.sortBy (withView snd ordering) values

-- | generate an unused Index
-- (newIndex l) `elem` l == False
newIndex :: [Index] -> Index
newIndex [] = 0
newIndex l = maximum l + 1

-- * constructors

(<:) :: a -> Indexable a -> Indexable a
a <: (Indexable values) =
    Indexable ((i, a) : values)
  where
    i = newIndex $ map fst values

(>:) :: Indexable a -> a -> Indexable a
(Indexable values) >: a =
    Indexable (values +: (i, a))
  where
    i = newIndex $ map fst values

fromList :: [a] -> Indexable a
fromList list = Indexable $ zip [0 ..] list

-- * mods

deleteByIndex :: Index -> Indexable a -> Indexable a
deleteByIndex i (Indexable values) =
    Indexable $ inner values
  where
    inner ((k, a) : r) | i == k = r
    inner (a : r) = a : inner r
    inner [] = error "deleteByIndex: index not found"

indexA :: Index -> Accessor (Indexable a) a
indexA i = accessor getter setter
  where
    getter ix = ix !!! i
    setter e (Indexable values) = Indexable $ inner values
      where
        inner ((k, a) : r) | k == i =
            (k, e) : r
        inner (a : r) = a : inner r

-- | puts the indexed element first
toHead :: Index -> Indexable a -> Indexable a
toHead i (Indexable values) =
    Indexable $ inner [] values
  where
    inner akk ((k, a) : r) | k == i =
        (k, a) : reverse akk ++ r
    inner akk (a : r) =
        inner (a : akk) r

-- | puts the indexed element last
toLast :: Index -> Indexable a -> Indexable a 
toLast i (Indexable values) =
    Indexable $ inner [] values
  where
    inner akk ((k, a) : r) | k == i =
        reverse akk ++ r +: (k, a)
    inner akk (a : r) =
        inner (a : akk) r


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
        let r = mergeListSome p list
        in if List.length r == List.length list then 
            list
          else
            fixpoint (n + 1) r


    convertToList :: Indexable a -> [Either (Index, a) a] -- left unmerged, right merged
    convertToList ix = map Left $ values ix
    convertToIndexable :: [Either (Index, a) a] -> Indexable a
    convertToIndexable list =
        Indexable $ zipWith inner list newIndices
      where
        newIndices = if null allIndices then [0..] else [maximum allIndices + 1..]
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
