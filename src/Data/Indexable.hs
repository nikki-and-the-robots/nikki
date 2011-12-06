{-# language GeneralizedNewtypeDeriving, ViewPatterns,
    DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

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

    Data.Indexable.length,
    Data.Foldable.toList,
    indexableNumber,
    (!!!),
    Data.Indexable.findIndices,
    Data.Indexable.filter,
    Data.Indexable.catMaybes,
    sortBy,

    Data.Indexable.fromList,
    (<:),
    (>:),
    append,
    insert,
    insertBefore,
    insertAfter,

    fmapMWithIndex,

    deleteByIndex,
    indexA,
    toHead,
    toLast,
    beforeIndex,
    isIndexOf,
    nextIndex,
    previousIndex,

    optimizeMerge,
  ) where

import Prelude hiding ((++), filter, reverse, elem, maximum, zip, zipWith, null, length, head, tail, concatMap)
import Safe

import qualified Data.List as List
import Data.Accessor
import Data.Foldable (Foldable(..), toList)
import Data.Initial
import Data.Traversable (Traversable)
import Data.Vector as Vector
import Data.Generics (Typeable, Data)
import Data.Maybe

import Control.Arrow

import Utils hiding (singleton)


newtype Index = Index {index :: Int}
  deriving (Show, Read, Enum, Num, Eq, Integral, Real, Ord, Data, Typeable)


-- | invariants:
-- sort (keys x) == sort (Map.keys (values x))
-- nub (keys x) == keys x
-- const True (keys x == sort (keys x))  (that is, the keys may be unsorted)
data Indexable a =
    Indexable {
        values :: (Vector (Index, a))
      }
  deriving (Eq, Typeable, Data, Functor, Foldable, Traversable)

instance Show a => Show (Indexable a) where
    show (Indexable v) = "Indexable " List.++ show (Vector.toList v)

instance Read a => Read (Indexable a) where
    readsPrec n s =
        if consString `List.isPrefixOf` s then
            List.map (first (Indexable . Vector.fromList)) $
                readsPrec n (List.drop (List.length consString) s)
        else
            error "Data.Indexable.readsPrec: not parseable"
      where
        consString = "Indexable "

keysVector :: Indexable a -> Vector Index
keysVector = fmap fst . values

keys :: Indexable a -> [Index]
keys = Vector.toList . keysVector



-- * instances

fmapMWithIndex :: (Monad m, Functor m) => (Index -> a -> m b)
    -> Indexable a -> m (Indexable b)
fmapMWithIndex cmd (Indexable values) = 
    Indexable <$> fmapM (\ (i, v) -> tuple i <$> cmd i v) values

instance Initial (Indexable a) where
    initial = Indexable empty

-- * getter

-- | returns the length of the contained list
length :: Indexable a -> Int
length = Vector.length . values

-- | returns, if the Index points to something
isIndexOf :: Index -> Indexable a -> Bool
isIndexOf i indexable = i `elem` keysVector indexable

-- | returns the number (according to the ordering) of the element of the given index.
indexableNumber :: Indexable a -> Index -> Int
indexableNumber (Indexable vector) i =
    case findIndex ((== i) . fst) vector of
        Just n -> n
        Nothing -> error "indexableNumber: Index not found"

-- | Returns all elements before the given index
-- (excluding the one referred to by the index).
beforeIndex :: Index -> Indexable a -> [a]
beforeIndex i =
    values >>>
    Vector.takeWhile ((/= i) . fst) >>>
    Vector.toList >>>
    fmap snd

-- | Returns the Index of the element after the one referred to by the given Index.
nextIndex :: Indexable a -> Index -> Maybe Index
nextIndex ix i =
    inner $ keys ix
  where
    inner (a : r) = if a == i then headMay r else inner r
    inner [] = Nothing

previousIndex :: Indexable a -> Index -> Maybe Index
previousIndex ix i =
    inner $ keys ix
  where
    inner (a : _) | a == i = Nothing
    inner (a : b : r) = if b == i then Just a else inner (b : r)
    inner _ = Nothing

(!!!) :: Indexable a -> Index -> a
(Indexable values) !!! i =
    case find ((== i) . fst) values of
        Just x -> snd x
        Nothing -> error ("!!!: Index not found")

-- | returns the list of indices for which the corresponding
-- values fullfill a given predicate.
-- Honours the order of values.
findIndices :: (a -> Bool) -> Indexable a -> [Index]
findIndices p (Indexable values) =
    Vector.toList $ fmap fst $ Vector.filter (p . snd) values

filter :: (a -> Bool) -> Indexable a -> Indexable a
filter p (Indexable values) =
    Indexable $ Vector.filter (p . snd) values

catMaybes :: Indexable (Maybe a) -> Indexable a
catMaybes = Data.Indexable.filter isJust >>> fmap fromJust

-- | Stable sorting of Indexables while preserving indices.
sortBy :: (a -> a -> Ordering) -> Indexable a -> Indexable a
sortBy ordering (Indexable values) =
    Indexable $ Vector.fromList $ List.sortBy (ordering `on` snd) $ Vector.toList values

-- | generate an unused Index
-- (newIndex l) `elem` l == False
newIndex :: Vector Index -> Index
newIndex l = if null l then 0 else maximum l + 1

-- * constructors

(<:) :: a -> Indexable a -> Indexable a
a <: (Indexable values) =
    Indexable ((i, a) `cons` values)
  where
    i = newIndex $ fmap fst values

(>:) :: Indexable a -> a -> Indexable a
(Indexable values) >: a =
    Indexable (values `snoc` (i, a))
  where
    i = newIndex $ fmap fst values

append :: Indexable a -> [a] -> Indexable a
append ix = List.foldl (>:) ix

-- | inserts an element (at the end) and returns the new Index
insert :: a -> Indexable a -> (Index, Indexable a)
insert a (Indexable values) =
    (i, Indexable ((i, a) `cons` values))
  where
    i = newIndex $ fmap fst values

-- | O(n)
-- Inserts a given element before the one with the given index.
insertBefore :: Index -> a -> Indexable a -> Indexable a
insertBefore searched new ix =
    inner ix
  where
    inner =
        values >>>
        concatMap (\ (i, a) ->
            if i == searched
            then (newI, new) `cons` singleton (i, a)
            else singleton (i, a)) >>>
        Indexable
    newI = newIndex $ fmap fst $ values ix

-- | O(n)
-- Inserts a given element after the one with the given index.
insertAfter :: Index -> a -> Indexable a -> Indexable a
insertAfter searched new ix =
    inner ix
  where
    inner =
        values >>>
        concatMap (\ (i, a) ->
            if i == searched
            then (i, a) `cons` singleton (newI, new)
            else singleton (i, a)) >>>
        Indexable
    newI = newIndex $ fmap fst $ values ix

fromList :: [a] -> Indexable a
fromList list = Indexable $ Vector.fromList (List.zip [0 ..] list)

-- * mods

deleteByIndex :: Index -> Indexable a -> Indexable a
deleteByIndex i (Indexable values) =
    Indexable $ Vector.filter (fst >>> (/= i)) values

indexA :: Index -> Accessor (Indexable a) a
indexA i = accessor getter setter
  where
    getter ix = ix !!! i
    setter e (Indexable values) = Indexable $ update values (Vector.singleton (foundVectorIndex, (i, e)))
      where
        (Just foundVectorIndex) = findIndex (fst >>> (== i)) values

-- | Puts the indexed element first.
-- Unsafe when Index not contained.
-- OPT: Vector-like?
toHead :: Index -> Indexable a -> Indexable a
toHead i (Indexable values) =
    Indexable $ inner empty values
  where
    inner :: Vector (Index, a) -> Vector (Index, a) -> Vector (Index, a)
    inner akk vector = case uncons vector of
        Just (a, r) ->
            if fst a == i then
                a `cons` reverse akk ++ r
            else
                inner (a `cons` akk) r

-- | Puts the indexed element last.
-- Unsafe when Index not contained.
toLast :: Index -> Indexable a -> Indexable a 
toLast i (Indexable values) =
    Indexable $ inner empty values
  where
    inner :: Vector (Index, a) -> Vector (Index, a) -> Vector (Index, a)
    inner akk vector = case uncons vector of
        Just (a, r) ->
            if fst a == i then
                reverse akk ++ r `snoc` a
            else
                inner (a `cons` akk) r


type MergeVector a = Vector (Either (Index, a) a) -- left unmerged, right merged

-- | optimizes an Indexable with merging.
-- calls the given function for every pair in the Indexable.
-- the given function returns Nothing, if nothing can be optimized and
-- returns the replacement for the optimized pair.
-- The old pair will be replaced with dummy elements.
-- This function is idempotent. (if that's an english word)
-- Note, that indices of optimized items are going to be invalidated.
optimizeMerge :: Show a => (a -> a -> Maybe a) -> Indexable a -> Indexable a
optimizeMerge p =
    convertToVector >>> fixpoint (mergeVectorSome p) >>> convertToIndexable
  where
    fixpoint :: (MergeVector a -> MergeVector a) -> MergeVector a -> MergeVector a
    fixpoint f vector =
        let r = f vector
        in if Vector.length r == Vector.length vector then 
            vector
          else
            fixpoint f r


    convertToVector :: Indexable a -> MergeVector a -- left unmerged, right merged
    convertToVector ix = fmap Left $ values ix
    convertToIndexable :: MergeVector a -> Indexable a
    convertToIndexable list =
        Indexable $ zipWith inner list newIndices
      where
        newIndices :: Vector Index
        newIndices = Vector.fromList $
            if null allIndices then [0 ..] else [maximum allIndices + 1 ..]
        allIndices = fmap fst $ lefts list
        inner (Left x) _ = x
        inner (Right x) i = (i, x)

-- OPT: This is probably not very Vector-like code.
mergeVectorSome :: (a -> a -> Maybe a) -> MergeVector a -> MergeVector a
mergeVectorSome p vector = case uncons vector of
    Just (a, r) ->
        case mergeSome p a (empty, r) of
            Just (merged, r') -> Right merged `cons` mergeVectorSome p r'
            Nothing -> a `cons` mergeVectorSome p r
    Nothing -> empty
  where
    mergeSome :: (a -> a -> Maybe a) -> Either (Index, a) a -> (MergeVector a, MergeVector a)
        -> Maybe (a, MergeVector a)
    mergeSome p outerA (before, after) = case uncons after of
        Just (outerB, r) ->
            case p (getInner outerA) (getInner outerB) of
                Just x -> Just (x, reverse before ++ r)
                Nothing -> mergeSome p outerA (outerB `cons` before, r)
        Nothing -> Nothing

    getInner (Left (_, a)) = a
    getInner (Right b) = b


-- * vector utils

lefts :: Vector (Either a b) -> Vector a
lefts = Vector.filter isLeft >>> fmap fromLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
    fromLeft (Left a) = a

-- | (head a, tail a) for mimicking (a : r) pattern matching
uncons :: Vector a -> Maybe (a, Vector a)
uncons v =
    if null v then Nothing else Just (head v, tail v)
