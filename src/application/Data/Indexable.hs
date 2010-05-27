{-# language GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

-- import qualified Data.Indexable as I
-- import Data.Indexable hiding (length, toList, findIndices, fromList, empty)

module Data.Indexable (
    Indexable,
    Index,

    Data.Indexable.length,
    toList,
    (!!!),
    Data.Indexable.findIndices,

    fromList,
    empty,
    (>:),
    (<:),

    fmapMWithIndex,

    modifyByIndex,
    modifyByIndexM,
    Data.Indexable.deleteByIndex,
    isNormalized,
    normalize,
    toHead,
    isIndexOf,

    optimizeMerge,
  ) where

import Utils

import Data.Map ()
import Data.List as List
import Control.Monad.FunctorM
import Data.Generics
import Data.Binary

import Control.Applicative ((<$>))


newtype Index = Index Int
  deriving (Show, Enum, Num, Eq, Read, Integral, Real, Ord, Data, Typeable)

data Indexable a = Indexable [Indexed a]
    deriving (Show, Read, Data, Typeable)

data Indexed a
    = Existent a
    | Deleted
    | Optimized
  deriving (Show, Read, Data, Typeable)

-- * instances

instance Binary Index where
    put (Index x) = put x
    get = Index <$> get

instance Functor Indexable where
    fmap f (Indexable c) = Indexable (fmap (fmap f) c)

instance FunctorM Indexable where
    fmapM f (Indexable c) = do
        x <- fmapM (fmapM f) c
        return $ Indexable x
    fmapM_ f (Indexable c) = mapM_ (fmapM_ f) c

instance Binary a => Binary (Indexable a) where
    put (Indexable x) = put x
    get = Indexable <$> get

instance Functor Indexed where
    fmap f (Existent a) = Existent $ f a
    fmap _ Deleted = Deleted
    fmap _ Optimized = Optimized

instance FunctorM Indexed where
    fmapM f (Existent a) =
        f a >>= return . Existent
    fmapM f Optimized = return Optimized
    fmapM f Deleted = return Deleted
    fmapM_ f (Existent a) =
        f a >> return ()
    fmapM_ _ _ = return ()

fmapMWithIndex :: Monad m => (Index -> a -> m b) -> Indexable a -> m (Indexable b)
fmapMWithIndex f (Indexable list) = inner 0 list ~> Indexable
  where
    inner i (a : r) = do
        a' <- singleInner i a
        r' <- inner (i + 1) r
        return (a' : r')
    inner i [] = return []
    singleInner i (Existent a) = f i a ~> Existent
    singleInner i Deleted = return Deleted
    singleInner i Optimized = return Optimized


instance Binary a => Binary (Indexed a) where
    put (Existent x) = do
        putWord8 0
        put x
    put Deleted = putWord8 1
    put Optimized = putWord8 2
    get = do
        c <- getWord8
        case c of
            0 -> Existent <$> get
            1 -> return Deleted
            2 -> return Optimized


-- * discriminators

isExistent :: Indexed a -> Bool
isExistent Existent{} = True
isExistent _ = False

-- * getter

-- | returns the length of the contained list (with all Deleteds and Optimizeds)
length :: Indexable a -> Index
length (Indexable ll) = Index (List.length ll)

-- | counts, how many Existent elements there are
howManyIndexables :: Indexable a -> Int
howManyIndexables (Indexable list) = List.length $ filter isExistent list

-- | returns, if the Index is in range of the Indexable (not, if the thing exists)
isIndexOf :: Index -> Indexable a -> Bool
isIndexOf index indexable = index >= 0 && index < Data.Indexable.length indexable

toList :: Indexable a -> [a]
toList (Indexable ll) = catExistent ll

catExistent :: [Indexed a] -> [a]
catExistent (Existent a : r) = a : catExistent r
catExistent (_ : r) = catExistent r
catExistent [] = []

(!!!) :: Indexable a -> Index -> a
(Indexable c) !!! (Index i) =
    case c !! i of
        Existent x -> x

findIndices :: (a -> Bool) -> Indexable a -> [Index]
findIndices p (Indexable ll) =
    map Index $ List.findIndices inner ll
  where
    inner (Existent x) = p x
    inner _ = False

-- * constructors

empty :: Indexable a
empty = Indexable []

(<:) :: a -> Indexable a -> Indexable a
a <: (Indexable ll)  = Indexable (Existent a : ll)

(>:) :: Indexable a -> a -> Indexable a
(Indexable ll) >: a = Indexable (ll +: Existent a)

fromList :: [a] -> Indexable a
fromList = foldr (<:) empty

-- * mods

deleteByIndex :: Indexable a -> Index -> Indexable a
deleteByIndex (Indexable ll) (Index i) = Indexable (inner ll i)
  where
    inner (Existent x : r) 0 = Deleted : r
    inner (a : r) i = a : inner r (i - 1)

modifyByIndex :: (a -> a) -> Index -> Indexable a -> Indexable a
modifyByIndex f (Index i) (Indexable ll) = Indexable new
  where
    new = inner ll i f

    inner :: [Indexed a] -> Int -> (a -> a) -> [Indexed a]
    inner (Existent x : r) 0 f = Existent (f x) : r
    inner (a : r) n f = a : inner r (n - 1) f

modifyByIndexM :: Monad m => (a -> m a) -> Index -> Indexable a -> m (Indexable a)
modifyByIndexM f (Index i) (Indexable list) =
    inner f i list ~> Indexable
  where
    inner :: Monad m => (a -> m a) -> Int -> [Indexed a] -> m [Indexed a]
    inner f 0 (Existent x : r) = do
        x' <- f x
        return $ Existent x' : r
    inner f n (a : r) =
        inner f (n - 1) r ~> (a :)

isNormalized :: Indexable a -> Bool
isNormalized (Indexable ll) = all isExistent ll

normalize :: Data d => Indexable a -> (Indexable a, d -> d)
normalize (Indexable []) = (Indexable [], id)
normalize (Indexable ll) =
    let abbildung = map sum $ init $ inits $ map mySignum ll
        mySignum (Existent _) = 1
        mySignum _ = 0
        indexFun :: Index -> Index
        indexFun (Index i) = Index (abbildung !! i)
        fun :: Data d => d -> d
        fun = everywhere (mkT indexFun)

        newIndexable = Indexable $ map Existent $ catExistent ll
    in (newIndexable, fun)

-- | puts the indexed element at the front
-- and returns a correction function for indices
-- pointing to the indexable
toHead :: Index -> Indexable a -> (Indexable a, d -> d)
toHead (Index i) (Indexable list) =
    (Indexable list', error "fun (toBack)")
  where
    list' = list !! i : (take i list ++ drop (i + 1) list)

    innerFun :: Index -> Index
    innerFun x | x < Index i = x + 1
    innerFun x | x == Index i = 0
    innerFun x | x > Index i = x
    fun :: Data d => d -> d
    fun = everywhere (mkT innerFun)

testNormalize :: IO ()
testNormalize =
    let expected = (Indexable [Existent 0, Existent 1, Existent 2, Existent 3], [Index 1, Index 3])
        got = (result, fun inputIndices)
        inputIndices = [Index 2, Index 6]
        (result, fun) = normalize inputList
        inputList = Indexable [Existent 0, Deleted, Existent 1, Optimized, Existent 2, Deleted, Existent 3]

        resolve ll is = map (ll !!!) is
        inputRefereds = resolve inputList inputIndices
        outputRefereds = uncurry resolve got
        evaluation = unlines [
            show inputRefereds,
            show outputRefereds,
            show expected,
            show got,
            show (show expected == show got)
          ]
    in putStrLn evaluation

testNormalize2 :: IO ()
testNormalize2 =
    let expected = (Indexable [Existent 0, Existent 1, Existent 2, Existent 3], [Index 1, Index 3])
        got = (result, fun inputIndices)
        inputIndices = [Index 1, Index 3]
        (result, fun) = normalize inputList
        inputList = Indexable [Existent 0, Existent 1, Existent 2, Existent 3]

        resolve ll is = map (ll !!!) is
        inputRefereds = resolve inputList inputIndices
        outputRefereds = uncurry resolve got
        evaluation = unlines [
            show inputRefereds,
            show outputRefereds,
            show expected,
            show got,
            show (show expected == show got)
          ]
    in putStrLn evaluation

-- | optimizes an Indexable with merging.
-- calls the given function for every pair in the Indexable.
-- the given function returns Nothing, if nothing can be optimized and
-- returns the replacement for the optimized pair.
-- The old pair will be replaced with dummy elements.
-- This function is idempotent. (if that's an english word)
optimizeMerge :: Show a => (a -> a -> Maybe a) -> Indexable a -> Indexable a
optimizeMerge f ix@(Indexable list) =
    if howManyIndexables ix /= howManyIndexables result then
        optimizeMerge f result
      else
        result
  where
    result = Indexable (looped list)
    looped list =
        let (changed, list') = iterate list
        in if changed then looped list' else list'

--     iterate :: Show a => [Maybe a] -> (Bool, [Maybe a])
    iterate (Existent a : r) =
        let (mMerged, r') = iterateSnd a r
        in case mMerged of
            Nothing -> modifySnd (Existent a :) (iterate r)
            Just merged -> (True, Optimized : r' +: Existent merged)
    iterate (a : r) = modifySnd (a :) (iterate r)
    iterate [] = (False, [])
--     iterate list = es "iterate(optimizeMerge)" list

--     iterateSnd :: Show a => a -> [Maybe a] -> (Maybe a, [Maybe a])
    iterateSnd a (Existent b : r) =
        case f a b of
            Nothing -> modifySnd (Existent b :) (iterateSnd a r)
            Just merged -> (Just merged, Optimized : r)
    iterateSnd a (b : r) = modifySnd (b :) (iterateSnd a r)
    iterateSnd a [] = (Nothing, [])
--     iterateSnd a list = es "iterateSnd(optimizeMerge)" (list)

