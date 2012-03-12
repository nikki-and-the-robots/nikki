{-# language ViewPatterns, ScopedTypeVariables, PackageImports, EmptyDataDecls, TypeSynonymInstances,
    FlexibleInstances #-}

module Utils (
    (<$>),
    (<*>),
    (<|>),
    (*>),
    (<*),
    pure,
    (>>>),
    (>=>),
    forM,
    forM_,
    when,
    (^.),
    (^=),
    (^:),
    (.>),
    module Utils,
    module Utils.Scripting,

    -- * accessor re-exports
    Accessor,
    (%=),
    (%:),

    -- * other exports
    on,
    Pair(..),
  ) where

-- imports

import Prelude hiding (catch)

import Safe

import Data.List
import Data.Map (Map, fromList, member, (!), findWithDefault, toList)
import Data.Foldable (Foldable, mapM_, forM_, any, sum)
import qualified Data.Foldable as Foldable
import Data.Traversable (Traversable, mapM)
import Data.IORef
import qualified Data.Set as Set
import Data.Accessor (Accessor, accessor, (^.), (^=), (^:), (.>))
import Data.Accessor.Monad.MTL.State ((%=), (%:))
import Data.Monoid
import Data.Function
import qualified Data.Strict as Strict
import Data.Strict (Pair(..))
import Data.Time.Clock.POSIX

import Text.Printf
import Text.Logging

import Control.Applicative ((<$>), (<|>), (<*>), (*>), (<*), pure, Alternative(..), Applicative)
import "mtl" Control.Monad.State hiding (forM_)
import "transformers" Control.Monad.Trans.Error (ErrorT(..))
                                                -- and Monad (Either e)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Arrow ((>>>))
import Control.Concurrent
import Control.Exception

import System.IO.Unsafe
import System.FilePath

import Utils.Scripting


-- * debugging stuff

data Todo
todo :: Todo
todo = error "just working on this (Utils.todo)"

todoError :: String -> a
todoError = error


-- | can be used to try out different values for constants without recompiling
tweakValue :: Read a => FilePath -> a
tweakValue file = System.IO.Unsafe.unsafePerformIO $ do
    value <- readFile file
    logg Info (file ++ " = " ++ value)
    return $ case readMay value of
        Nothing -> error ("cannot read: " ++ value)
        Just x -> x
{-# noinline tweakValue #-}

-- | prints debug messages (as unsafe side effects)
-- can be used at the end of expressions like this:
-- (x * y + 3) << id
-- something << getter
(<<) :: Show s => a -> (a -> s) -> a
a << f = trace (show $ f a) a

-- | prints out an expression as a debugging message (unsafe side effect)
-- with a given message 
(<<?) :: Show a => a -> String -> a
a <<? msg = trace (msg ++ ": " ++ show a) a


-- | useful for temporarily deactivating $<<?$
(<<|) :: Show a => a -> String -> a
a <<| _ = a

-- | re-implementation of trace that uses Text.Logging.logg
trace :: String -> a -> a
trace msg x = unsafePerformIO $ do
    logg Debug msg
    return x

traceThis :: String -> (x -> String) -> x -> x
traceThis "" showFun x = trace (showFun x) x
traceThis msg showFun x = trace (msg ++ ": " ++ showFun x) x



e :: String -> a
e = error

es :: Show s => String -> s -> a
es msg x = error (msg ++ ": " ++ show x)

nm :: Show s => String -> s -> a
nm msg = es ("Non-exhaustive patterns: " ++ msg)


assertIO :: Bool -> String -> IO ()
assertIO True _ = return ()
assertIO False msg = error ("ASSERTION ERROR: " ++ msg)

-- | returns True every n-th time 'every' is called.
-- (of course this involves unsafeIO-magick.
every :: Int -> IO () -> IO ()
every n cmd = do
    c <- readIORef everyRef
    if c >= n then do
        writeIORef everyRef 0
        cmd
      else do
        writeIORef everyRef (c + 1)
        return ()

{-# NOINLINE everyRef #-}
everyRef :: IORef Int
everyRef = unsafePerformIO $ newIORef 0

-- | @wait n@ waits for n seconds
wait :: MonadIO m => Double -> m ()
wait n = io $ threadDelay $ round (n * 10 ^ 6)


-- * re-named re-exports

fmapM :: (Data.Traversable.Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
fmapM = Data.Traversable.mapM

fmapM_ :: (Monad m, Data.Foldable.Foldable t) => (a -> m b) -> t a -> m ()
fmapM_ = Data.Foldable.mapM_

fany :: Foldable t => (a -> Bool) -> t a -> Bool
fany = Data.Foldable.any

fsum :: (Foldable t, Num a) => t a -> a
fsum = Data.Foldable.sum

-- | Efficiency talk: Both flength and fnull assume that it is
-- faster to access the first elements than the last.
flength :: (Functor t, Foldable t) => t a -> Int
flength = Foldable.foldl (+) 0 . fmap (const 1)

fnull :: (Functor t, Foldable t) => t a -> Bool
fnull = Foldable.foldr (&&) True . fmap (const False)

ftoList :: Foldable f => f a -> [a]
ftoList = Foldable.toList

(+>) :: Monoid m => m -> m -> m
(+>) = mappend


-- * function composition stuff

(|>) :: a -> (a -> b) -> b
a |> f = f a

-- fake kleisli stuff

passThrough :: Monad m => (a -> m ()) -> (a -> m a)
passThrough cmd a = cmd a >> return a

secondKleisli :: Functor f => (a -> f b) -> ((x, a) -> f (x, b))
secondKleisli cmd (x, a) =
    fmap (tuple x) $ cmd a

(<>>) :: Functor m => m a -> (a -> b) -> m b
action <>> f = f <$> action


-- * State monad stuff

puts :: MonadState s m => (a -> s -> s) -> a -> m ()
puts setter a = do
    s <- get
    put (setter a s)

modifies :: MonadState s m => (s -> a) -> (a -> s -> s) -> (a -> a) -> m ()
modifies getter setter fun = do
    a <- gets getter
    puts setter (fun a)

modifiesT :: (Monad m, MonadTrans t, MonadState s (t m)) =>
    (s -> a) -> (a1 -> s -> s) -> (a -> m a1) -> t m ()
modifiesT getter setter cmd = do
    x <- gets getter
    x' <- lift $ cmd x
    puts setter x'

modifyState :: MonadState s m => (s -> s) -> m ()
modifyState f =
    get >>= (return . f) >>= put

-- | runs a state monad on the content of an IORef
-- (useful for embedding state monad in e.g callback functions)
runStateTFromIORef :: IORef s -> StateT s IO a -> IO a
runStateTFromIORef ref cmd = do
    s <- readIORef ref
    (o, s') <- runStateT cmd s
    writeIORef ref s'
    return o

-- | is not atomic
modifyIORefM :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM ref cmd =
    readIORef ref >>=
    cmd >>=
    writeIORef ref
    

-- * mvar stuff

tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar mvar = do
    r <- tryTakeMVar mvar
    forM_ r $ putMVar mvar
    return r


-- * Monad stuff

chainAppM :: Monad m => (b -> a -> m a) -> [b] -> a -> m a
chainAppM cmd (b : r) a = do
    a' <- cmd b a
    chainAppM cmd r a'
chainAppM _ [] a = return a

ignore :: Monad m => m a -> m ()
ignore = (>> return ())

{-# inline io #-}
io :: MonadIO m => IO a -> m a
io = liftIO

-- applies a given monadic operation n times
applyTimesM :: Monad m => Int -> (a -> m a) -> a -> m a
applyTimesM 0 m = return
applyTimesM n m =
    m >=> applyTimesM (pred n) m

infixl 8 ^^:
(^^:) :: Functor m => Accessor r a -> (a -> m a) -> r -> m r
acc ^^: f = \ r ->
    fmap (\ a' -> acc ^= a' $ r) (f (r ^. acc))

(>$>) :: Functor m => m a -> (a -> b) -> m b
(>$>) = flip fmap

catchSomeExceptionsErrorT :: MonadCatchIO m =>
    (SomeException -> e) -> ErrorT e m a -> ErrorT e m a
catchSomeExceptionsErrorT convert (ErrorT cmd) =
    ErrorT $ Control.Monad.CatchIO.catch cmd (return . Left . convert)

convertErrorT :: Functor m => (a -> b) -> ErrorT a m o -> ErrorT b m o
convertErrorT f (ErrorT action) = ErrorT $
    (either (Left . f) Right <$> action)


-- * either stuff

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right c) = Right c


-- * list stuff

infixl 4 +:
(+:) :: [a] -> a -> [a]
a +: b = a ++ [b]

singleton :: a -> [a]
singleton = (: [])

-- | dropPrefix a b drops the longest prefix from b that is equal to a prefix of a.
dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix (a : aR) (b : bR) =
    if a == b then dropPrefix aR bR else b : bR
dropPrefix _ b = b

-- | drops the given prefix of a list, if prefix `isPrefixOf` list.
dropPrefixMay :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefixMay prefix list =
    if prefix `isPrefixOf` list then
        Just $ drop (length prefix) list
      else
        Nothing


chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n l =
    let (a, b) = splitAt n l
    in a : chunks n b

-- returns the list of items that are in the given list more than once
duplicates :: (Eq a, Ord a) => [a] -> [a]
duplicates =
    nub . inner Set.empty
  where
    inner elements (a : r) =
        if Set.member a elements then a : rest else rest
      where
        rest = inner (Set.insert a elements) r
    inner _ [] = []

chainApp :: (b -> a -> a) -> [b] -> a -> a
chainApp fun (b : r) a = chainApp fun r (fun b a)
chainApp _ [] a = a

toEitherList :: [a] -> [b] -> [Either a b]
toEitherList as bs = map Left as ++ map Right bs

single :: String -> [a] -> a
single _ [a] = a
single msg [] = error ("empty list in single: " ++ msg)
single msg __ = error ("more than one element in list in single: " ++ msg)

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

wordsBy :: Eq a => [a] -> [a] -> [[a]]
wordsBy seps ll = inner [] ll
  where
    inner akk [] = [reverse akk]
    inner akk (a : r) =
        if a `elem` seps then
            reverse akk : wordsBy seps r
          else
            inner (a : akk) r

cartesian :: [a] -> [b] -> [(a, b)]
cartesian al bl =
    concatMap (\ b -> map (\ a -> (a, b)) al) bl

-- | returns every combination of given elements once.
completeEdges :: [a] -> [(a, a)]
completeEdges (a : r) = map (tuple a) r ++ completeEdges r
completeEdges [] = []

adjacentCyclic :: [a] -> [(a, a)]
adjacentCyclic [] = []
adjacentCyclic [_] = []
adjacentCyclic list@(head : _) = inner head list
  where
    inner first (a : b : r) = (a, b) : inner first (b : r)
    inner first [last] = [(last, first)]
    inner _ [] = error "adjacentCyclic"

-- | merges pairs of elements for which the given function returns (Just a).
-- removes the pair and inserts (the merged) as.
-- Is idempotent.
mergePairs :: Eq a => (a -> a -> Maybe [a]) -> [a] -> [a]
mergePairs f =
    fixpoint merge
  where
    merge (a : r) =
        case inner a r of
            Nothing -> a : merge r
            Just r' -> r'
    merge [] = []
    inner a (b : r) =
        case f a b <|> f b a of
            Nothing ->
                case inner a r of
                    Nothing -> Nothing
                    Just r' -> Just (b : r')
            Just newAs -> Just (newAs ++ r)
    inner _ [] = Nothing

-- | like mergePairs, but only tries to merge adjacent elements
-- (or the first and the last element)
-- Is idempotent.
mergeAdjacentCyclicPairs :: Eq a => (a -> a -> Maybe a) -> [a] -> [a]
mergeAdjacentCyclicPairs f =
    fixpoint merge
  where
    merge = headAndLast . adjacent
    adjacent (a : b : r) = case f a b of
        Nothing -> a : adjacent (b : r)
        Just x -> adjacent (x : r)
    adjacent [x] = [x]
    adjacent [] = []
    headAndLast [] = []
    headAndLast [a] = [a]
    headAndLast l = case f (last l) (head l) of
        Nothing -> l
        Just x -> x : tail (init l)

-- | returns the local minima of a list.
localMinima :: Ord n => [n] -> [n]
localMinima (a : b : c : r) =
    if a > b && c > b then
        b : localMinima (c : r)
      else
        localMinima (b : c : r)
localMinima _ = []


-- * String stuff

-- | adds an extension, if the path does not already have the same extension
(<..>) :: FilePath -> String -> FilePath
path <..> ext =
    if dotExt `isSuffixOf` path then path else path <.> ext
  where
    dotExt = if Just '.' == headMay ext then ext else '.' : ext


-- * Map stuff

fromKeys :: Ord k => (k -> a) -> [k] -> Map k a
fromKeys f keys = fromList (map (\ key -> (key, f key)) keys)

lookups :: Ord k => [k] -> Map k a -> Maybe a
lookups [] _ = Nothing
lookups (k : r) m = if k `member` m then Just (m ! k) else lookups r m

-- | creates a mapping function with an error message
toFunction :: (Show k, Ord k) => String -> Map k e -> k -> e
toFunction msg m k = findWithDefault err k m
  where
    err = error ("key not found: " ++ show k ++ " from " ++ msg)

mapPairs :: Ord k => (k -> a -> (k, a)) -> Map k a -> Map k a
mapPairs f = fromList . map (uncurry f) . toList


-- * Maybe stuff

justWhen :: Bool -> a -> Maybe a
justWhen True = Just
justWhen False = const Nothing


-- * math stuff

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> x = x
infixr 2 ==>

(~=) :: (Ord n, Fractional n) => n -> n -> Bool
a ~= b = distance a b < epsilon

epsilon :: Fractional n => n
epsilon = 0.001

divide :: (RealFrac f, Integral i) => f -> f -> (i, f)
divide a b = (n, f * b)
  where
    (n, f) = properFraction (a / b)

-- | folds the given number to the given range
-- range is including lower bound and excluding upper bound
-- OPT: is O(a), could be constant (using properFraction)
foldToRange :: (Ord n, Num n) => (n, n) -> n -> n
foldToRange (lower, upper) _ | upper <= lower = e "foldToRange"
foldToRange (lower, upper) a | a >= upper = foldToRange (lower, upper) (a - distance lower upper)
foldToRange (lower, upper) a | a < lower = foldToRange (lower, upper) (a + distance lower upper)
foldToRange _ a = a

-- | returns, if two values are very near in a (floating) modulo body.
rangeEpsilonEquals :: (Ord n, Fractional n) => (n, n) -> n -> n -> Bool
rangeEpsilonEquals range a b =
    or (map (aR ~=) [bR, bR + diff, bR - diff])
  where
    diff = uncurry distance range
    aR = foldToRange range a
    bR = foldToRange range b

distance :: Num n => n -> n -> n
distance a b = abs (a - b)

-- | clips a number to a given range
-- range is including both bounds
clip :: (Ord n, Num n) => (n, n) -> n -> n
clip (lower, _) x | x < lower = lower
clip (lower, upper) x | x >= lower && x <= upper = x
clip (_, upper) _ = upper


-- * tuple stuff

tuple :: a -> b -> (a, b)
tuple a b = (a, b)

swapTuple :: (a, b) -> (b, a)
swapTuple (a, b) = (b, a)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, x) = x


-- * misc

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a, b, c, d) = f a b c d

swapOrdering :: Ordering -> Ordering
swapOrdering LT = GT
swapOrdering GT = LT
swapOrdering EQ = EQ

xor :: Bool -> Bool -> Bool
xor True True = False
xor a b = a || b

-- | boolean implication
infix 4 ~>
(~>) :: Bool -> Bool -> Bool
True ~> x = x
False ~> _ = True

infix 4 ~.>
(~.>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(a ~.> b) x = a x ~> b x

fixpoint :: Eq e => (e -> e) -> e -> e
fixpoint f x = if fx == x then x else fixpoint f fx
  where
    fx = f x

-- | applies a function n times
superApply :: Int -> (a -> a) -> a -> a
superApply n f = foldr (.) id $ replicate n f

-- | returns all possible values, sorted.
allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound .. maxBound]


-- * Pretty Printing

class PP a where
    pp :: a -> String

instance (PP a, PP b) => PP (a, b) where
    pp (a, b) = "(" ++ pp a ++ ", " ++ pp b ++ ")"

instance (PP a, PP b) => PP (Pair a b) where
    pp (a :!: b) = "(" ++ pp a ++ " :!: " ++ pp b ++ ")"

instance (PP a, PP b, PP c) => PP (a, b, c) where
    pp (a, b, c) = "(" ++ pp a ++ ", " ++ pp b ++ ", " ++ pp c ++ ")"

instance (PP a, PP b, PP c, PP d) => PP (a, b, c, d) where
    pp (a, b, c, d) = "(" ++ pp a ++ ", " ++ pp b ++ ", " ++ pp c ++ ", " ++ pp d ++ ")"

instance PP Bool where
    pp True = "|"
    pp False = "O"

instance PP a => PP [a] where
    pp list = "[" ++ intercalate ", " (map (clipString . pp) list) ++ "]"
      where
        clipString s = if length s < limit then s else take (limit - length dots) s ++ dots
        limit = 20
        dots = "..."

instance PP a => PP (Set.Set a) where
    pp set = "{" ++ (tail (init (pp (Set.toList set)))) ++ "}"

instance PP a => PP (Maybe a) where
    pp Nothing = "Nothing"
    pp (Just x) = "Just (" ++ pp x ++ ")"

instance PP a => PP (Strict.Maybe a) where
    pp Strict.Nothing = "Strict.Nothing"
    pp (Strict.Just x) = "Strict.Just (" ++ pp x ++ ")"

instance PP Double where
    pp = printf "%8.3f"

instance PP Float where
    pp = printf "%8.3f"

instance PP Int where
    pp = show

ppp :: PP p => p -> IO ()
ppp = pp >>> logg Info


-- misc

instance Applicative Strict.Maybe where
    pure = Strict.Just
    (Strict.Just f) <*> (Strict.Just x) = Strict.Just $ f x
    _ <*> _ = Strict.Nothing

instance Alternative Strict.Maybe where
    empty = Strict.Nothing
    Strict.Nothing <|> x = x
    (Strict.Just x) <|> _ = Strict.Just x

firstStrict :: (a -> b) -> (Pair a c) -> (Pair b c)
firstStrict f (a :!: c) = f a :!: c

firstAStrict :: Accessor (Pair a b) a
firstAStrict = accessor (\ (a :!: _) -> a) (\ a (_ :!: b) -> (a :!: b))

-- | Returns the current time in seconds.
getTime :: IO Double
getTime = realToFrac <$> getPOSIXTime
