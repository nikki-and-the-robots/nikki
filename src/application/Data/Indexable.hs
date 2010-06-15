{-# language GeneralizedNewtypeDeriving, DeriveDataTypeable, NamedFieldPuns,
     ViewPatterns #-}

-- | module for a Bag of indexed things. 
-- They have an order (can be converted to a list.)
-- imports could look like this:
--
-- import qualified Data.Indexable as I
-- import Data.Indexable hiding (length, toList, findIndices, fromList, empty)

module Data.Indexable (
    Indexable,
    Index(..),

    length,
    toList,
    (!!!),
    findIndices,

    fromList,
    empty,
    (<:),
    (>:),

    fmapMWithIndex,

    modifyByIndex,
--     modifyByIndexM,
    deleteByIndex,
    toHead,
    isIndexOf,

    optimizeMerge,
  ) where

import Utils hiding (deleteByIndex)

import Prelude hiding (length)

import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.List as List
import Data.Generics
import Data.Either

import Control.Monad.FunctorM


newtype Index = Index Int
  deriving (Show, Enum, Num, Eq, Read, Integral, Real, Ord, Data, Typeable)

-- | invariants:
-- sort (keys x) == sort (Map.keys (values x))
-- nub (keys x) == keys x
data Indexable a = Indexable {
    values :: Map.Map Index a,
    keys :: [Index]
  }
    deriving (Show, Read, Data, Typeable)


-- * instances

instance Functor Indexable where
    fmap f (Indexable values keys) = Indexable (fmap f values) keys

instance FunctorM Indexable where
    fmapM cmd (Indexable values keys) = do
        newValues <- fmapM (cmd . (values !)) keys
        return $ Indexable (Map.fromList $ zip keys newValues) keys
    fmapM_ cmd (Indexable values keys) =
        mapM_ (cmd . (values !)) keys

fmapMWithIndex :: Monad m => (Index -> a -> m b) -> Indexable a
    -> m (Indexable b)
fmapMWithIndex cmd (Indexable values keys) = do
    newValues <- mapM (\ k -> cmd k (values ! k)) keys
    return $ Indexable (Map.fromList $ zip keys newValues) keys
    


-- * getter

-- | returns the length of the contained list
length :: Indexable a -> Index
length = Index . List.length . keys

-- -- | returns, if the Index points to something
isIndexOf :: Index -> Indexable a -> Bool
isIndexOf i indexable = i `elem` keys indexable

toList :: Indexable a -> [a]
toList x = map ((values x) !) $ keys x

(!!!) :: Indexable a -> Index -> a
Indexable{values} !!! i =
    case Map.lookup i values of
        Just x -> x
        Nothing -> error ("!!!: Index not found")

-- | returns the list if indices for which the corresponding
-- values fullfill a given predicate.
-- Honours the order of values.
findIndices :: (a -> Bool) -> Indexable a -> [Index]
findIndices p (Indexable values keys) =
    filter (p . (values !)) keys

-- | generate an unused Index
-- (newIndex l) `elem` l == False
newIndex :: [Index] -> Index
newIndex [] = 0
newIndex l = maximum l + 1

-- * constructors

empty :: Indexable a
empty = Indexable Map.empty []

(<:) :: a -> Indexable a -> Indexable a
a <: (Indexable values keys) =
    Indexable (Map.insert i a values) (i : keys)
  where
    i = newIndex keys

(>:) :: Indexable a -> a -> Indexable a
(Indexable values keys) >: a =
    Indexable (Map.insert i a values) (keys +: i)
  where
    i = newIndex keys

fromList :: [a] -> Indexable a
fromList list = Indexable (Map.fromList pairs) $ map fst pairs
  where
    pairs = zip [0..] list

-- * mods

deleteByIndex :: Indexable a -> Index -> Indexable a
deleteByIndex (Indexable values keys) i =
    Indexable (Map.delete i values) (filter (/= i) keys)

modifyByIndex :: (a -> a) -> Index -> Indexable a -> Indexable a
modifyByIndex f i (Indexable values keys) | i `elem` keys =
    Indexable (Map.adjust f i values) keys

-- modifyByIndexM :: Monad m => (a -> m a) -> Index -> Indexable a -> m (Indexable a)
-- modifyByIndexM f (Index i) (Indexable list) =
--     inner f i list ~> Indexable
--   where
--     inner :: Monad m => (a -> m a) -> Int -> [Indexed a] -> m [Indexed a]
--     inner f 0 (Existent x : r) = do
--         x' <- f x
--         return $ Existent x' : r
--     inner f n (a : r) =
--         inner f (n - 1) r ~> (a :)

-- | puts the indexed element at the front
-- and returns a correction function for indices
-- pointing to the indexable
toHead :: Index -> Indexable a -> Indexable a
toHead i (Indexable values keys) | i `elem` keys =
    Indexable values (i : filter (/= i) keys)


-- | optimizes an Indexable with merging.
-- calls the given function for every pair in the Indexable.
-- the given function returns Nothing, if nothing can be optimized and
-- returns the replacement for the optimized pair.
-- The old pair will be replaced with dummy elements.
-- This function is idempotent. (if that's an english word)
-- Note, that indices of optimized items are going to be invalidated.
optimizeMerge :: Show a => (a -> a -> Maybe a) -> Indexable a -> Indexable a
optimizeMerge p ix =
    let r = mergeIndexableOnce p ix
    in if length r == length ix then ix else optimizeMerge p r

mergeIndexableOnce :: Show a => (a -> a -> Maybe a) -> Indexable a -> Indexable a
mergeIndexableOnce p =
    convertToList .> mergeListOnce p .> convertToIndexable
  where
    convertToList :: Indexable a -> [Either (Index, a) a] -- left unmerged, right merged
    convertToList ix = map (\ i -> Left (i, values ix ! i)) (keys ix)
    convertToIndexable :: [Either (Index, a) a] -> Indexable a
    convertToIndexable list =
        Indexable (Map.fromList newValues) (map fst newValues)
      where
        newValues = zipWith inner list newIndices
        newIndices = [maximum allIndices + 1..]
        allIndices = map fst $ lefts list
        inner (Left x) _ = x
        inner (Right x) i = (i, x)

    mergeListOnce :: Show a => (a -> a -> Maybe a)
        -> [(Either (Index, a) a)] -> [(Either (Index, a) a)]
    mergeListOnce p (a : r) =
        case mergeOnce p a ([], r) of
            Just (merged, r') -> Right merged : r'
            Nothing -> a : mergeListOnce p r
    mergeListOnce _ [] = []

    mergeOnce :: Show a => (a -> a -> Maybe a)
        -> Either (Index, a) a
        -> ([(Either (Index, a) a)], [(Either (Index, a) a)])
        -> Maybe (a, [(Either (Index, a) a)])
    mergeOnce p a_@(Left (_, a)) (before, (b_@(Left (_, b)) : r)) =
        case p a b of
            Just x -> Just (x, before ++ r)
            Nothing -> mergeOnce p a_ (before +: b_, r)
    mergeOnce _ _ (_, []) = Nothing
    mergeOnce p a b = es "help" (a, b)



--     if howManyIndexables ix /= howManyIndexables result then
--         optimizeMerge f result
--       else
--         result
--   where
--     result = Indexable (looped list)
--     looped list =
--         let (changed, list') = iterate list
--         in if changed then looped list' else list'
-- 
-- --     iterate :: Show a => [Maybe a] -> (Bool, [Maybe a])
--     iterate (Existent a : r) =
--         let (mMerged, r') = iterateSnd a r
--         in case mMerged of
--             Nothing -> modifySnd (Existent a :) (iterate r)
--             Just merged -> (True, Optimized : r' +: Existent merged)
--     iterate (a : r) = modifySnd (a :) (iterate r)
--     iterate [] = (False, [])
-- --     iterate list = es "iterate(optimizeMerge)" list
-- 
-- --     iterateSnd :: Show a => a -> [Maybe a] -> (Maybe a, [Maybe a])
--     iterateSnd a (Existent b : r) =
--         case f a b of
--             Nothing -> modifySnd (Existent b :) (iterateSnd a r)
--             Just merged -> (Just merged, Optimized : r)
--     iterateSnd a (b : r) = modifySnd (b :) (iterateSnd a r)
--     iterateSnd a [] = (Nothing, [])
-- --     iterateSnd a list = es "iterateSnd(optimizeMerge)" (list)
-- 
