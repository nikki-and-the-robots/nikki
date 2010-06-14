{-# language EmptyDataDecls, FlexibleInstances #-}

module Utils (
    (<$>),
    module Utils,
  ) where

-- imports

import Data.Char
import Data.List
import Data.Map (Map, fromList, member, (!), findWithDefault)

import Control.Applicative ((<$>))
import Control.Monad.State hiding ((>=>))

import System
import System.Directory
import System.IO.Unsafe
import System.FilePath

import Debug.Trace


-- * debugging stuff

data Todo
todo :: Todo
todo = error "just working on this (Utils.todo)"


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
a <<| _ = a



e :: String -> a
e = error

es :: Show s => String -> s -> a
es msg x = error (msg ++ ": " ++ show x)

nm :: Show s => String -> s -> a
nm msg = es ("Non-exhaustive patterns: " ++ msg)


{-# NOINLINE debug #-}
debug :: String -> a -> a
debug msg x = unsafePerformIO $ do
    putStrLn ("DEBUG: " ++ msg)
    return x

debugs :: Show s => String -> s -> a -> a
debugs msg s = debug (msg ++ ": " ++ show s)

printDebug :: String -> IO ()
printDebug msg = putStrLn ("\tDEBUG: " ++ msg)

assertIO :: Bool -> String -> IO ()
assertIO True _ = return ()
assertIO False msg = error ("ASSERTION ERROR: " ++ msg)

warn :: MonadIO m => String -> m ()
warn m = liftIO $ putStrLn ("WARNING: " ++ m)

toDebug :: Show s => String -> s -> String
toDebug msg s = msg ++ ": " ++ show s


-- * function and monad composition stuff

(|>) :: a -> (a -> b) -> b
a |> f = f a

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)

(.>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
x .>> y = \ a -> x a >>= y

(|>>) :: Monad m => m a -> (a -> b) -> m b
a |>> b = a >>= (return . b)


-- lifter stuff

pure :: Monad m => (a -> b) -> (a -> m b)
pure = (return .)

passThrough :: Monad m => (a -> m ()) -> (a -> m a)
passThrough cmd a = cmd a >> return a




-- * scripting stuff

-- | executes a unix command on the shell and exits if it does not succeed.
trySystem :: String -> IO ()
trySystem cmd = do
    putStrLn ("Executing \"" ++ cmd ++ "\" ...")
    exitcode <- system cmd
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure n -> exitWith $ ExitFailure n

-- | changes the working directory temporarily.
withCurrentDirectory :: FilePath -> IO () -> IO ()
withCurrentDirectory path cmd = do
    putStrLn ("Entering directory " ++ path)
    oldWorkingDirectory <- getCurrentDirectory
    setCurrentDirectory path
    x <- cmd
    setCurrentDirectory oldWorkingDirectory
    putStrLn ("Leaving directory " ++ path)
    return x

-- * State monad stuff

puts :: MonadState s m => (s -> a -> s) -> a -> m ()
puts setter a = do
    s <- get
    put (setter s a)

modifies :: MonadState s m => (s -> a) -> (s -> a -> s) -> (a -> a) -> m ()
modifies getter setter fun = do
    a <- gets getter
    puts setter (fun a)

modifiesT :: (Monad m, MonadTrans t, MonadState s (t m)) =>
    (s -> a) -> (s -> a1 -> s) -> (a -> m a1) -> t m ()
modifiesT getter setter cmd = do
    x <- gets getter
    x' <- lift $ cmd x
    puts setter x'


-- * Monad stuff

chainAppM :: Monad m => (b -> a -> m a) -> [b] -> a -> m a
chainAppM cmd (b : r) a = do
    a' <- cmd b a
    chainAppM cmd r a'
chainAppM _ [] a = return a


-- * list stuff
infixl 4 +:
(+:) :: [a] -> a -> [a]
a +: b = a ++ [b]

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

rotate :: [a] -> [a]
rotate [] = []
rotate (a : r) = r +: a

rotateBack :: [a] -> [a]
rotateBack [] = []
rotateBack ll = last ll : init ll

adjacentCyclic :: [a] -> [(a, a)]
adjacentCyclic [] = []
adjacentCyclic [a] = []
adjacentCyclic list@(head : _) = inner head list
  where
    inner first (a : b : r) = (a, b) : inner first (b : r)
    inner first [last] = [(last, first)]

-- | removes pairs of elements (both elements)
removePairs :: (a -> a -> Bool) -> [a] -> [a]
removePairs f = mergePairs $ \ a b -> if f a b then Just [] else Nothing

-- | merges pairs of elements for which the given function returns (Just a).
-- removes the pair and inserts (the merged) as.
mergePairs :: (a -> a -> Maybe [a]) -> [a] -> [a]
mergePairs f (a : r) =
    case inner a r of
        Nothing -> a : mergePairs f r
        Just r' -> mergePairs f r'
  where
--     inner :: a -> [a] -> Maybe [a]
    inner a (b : r) =
        case f a b of
            Nothing ->
                case inner a r of
                    Nothing -> Nothing
                    Just r' -> Just (b : r')
            Just newAs -> Just (newAs ++ r)
    inner a [] = Nothing
mergePairs f [] = []

-- | deletes an element by the given index
deleteByIndex :: Int -> [a] -> [a]
deleteByIndex 0 (_ : r) = r
deleteByIndex n (a : r) = a : deleteByIndex (n - 1) r
deleteByIndex n [] = e ("deleteByIndex " ++ show n ++ " []")

repeatList :: Int -> [a] -> [a]
repeatList 0 _ = []
repeatList n list = list ++ repeatList (n - 1) list

applyAll :: [a -> a] -> a -> a
applyAll fs a = foldl (.) id fs a


-- * String stuff

toCapital :: String -> String
toCapital [] = []
toCapital (a : r) = toUpper a : r

toNotCapital :: String -> String
toNotCapital [] = []
toNotCapital (a : r) = toLower a : r

-- * Map stuff

fromKeys :: Ord k => (k -> a) -> [k] -> Map k a
fromKeys f keys = fromList (map (\ key -> (key, f key)) keys)

lookups :: Ord k => [k] -> Map k a -> Maybe a
lookups [] m = Nothing
lookups (k : r) m = if k `member` m then Just (m ! k) else lookups r m

safeLookup :: Ord k => String -> Map k e -> k -> e
safeLookup msg m k | k `member` m = m ! k

-- | creates a mapping function with an error message
toFunction :: (Show k, Ord k) => String -> Map k e -> k -> e
toFunction msg m k = findWithDefault err k m
  where
    err = error ("key not found: " ++ show k ++ " from " ++ msg)


-- * Maybe stuff

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe mx cmd =
    case mx of
        Just x -> cmd x
        Nothing -> return ()

maybeId :: (a -> Maybe a) -> (a -> a)
maybeId fun a =
    case fun a of
        Nothing -> a
        Just x -> x

-- * math stuff

rad2deg :: Floating a => a -> a
rad2deg x = (x * 360) / (pi * 2)

cartesian :: [a] -> [b] -> [(a, b)]
cartesian a b = do
    a' <- a
    b' <- b
    return (a', b')

(~=) :: (Ord n, Fractional n) => n -> n -> Bool
a ~= b = distance a b < epsilon

epsilon :: Fractional n => n
epsilon = 0.001

divide :: Double -> Double -> (Int, Double)
divide a b = (n, f * b)
  where
    (n, f) = properFraction (a / b) 

-- | folds the given number to the given range
-- range is including lower bound and excluding upper bound
-- TODO: is O(a), could be constant (using properFraction)
foldToRange :: (Ord n, Num n) => (n, n) -> n -> n
foldToRange (lower, upper) a | upper <= lower = e "foldToRange"
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
clip (_, upper) x | x > upper = upper


-- * tuple stuff

tuple :: a -> b -> (a, b)
tuple a b = (a, b)

swapTuple :: (a, b) -> (b, a)
swapTuple (a, b) = (b, a)

modifySnd :: (a -> b) -> (x, a) -> (x, b)
modifySnd f (x, a) = (x, f a)


-- * misc

readE :: Read r => String -> r
readE r = case readM r of
    Right x -> x
    Left msg -> error msg

instance Monad (Either String) where
    (>>=) = error ">>="
    return = Right
    fail = Left

readM :: (Monad m, Read r) => String -> m r
readM x = unsafePerformIO $
    catch
        (return <$> readIO x)
        (\ e -> return (fail ("readM: no parse: " ++ take 100000 x)))

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a, b, c, d) = f a b c d

withView :: (a -> b) -> (b -> b -> c) -> a -> a -> c
withView view operator a b = operator (view a) (view b)

swapOrdering :: Ordering -> Ordering
swapOrdering LT = GT
swapOrdering GT = LT
swapOrdering EQ = EQ

xor :: Bool -> Bool -> Bool
xor True True = False
xor a b = a || b


-- * Pretty Printing

class PP a where
    pp :: a -> String

formatInt :: Int -> Int -> String
formatInt = e "formatInt"

formatDouble :: (Int, Int) -> Double -> String
formatDouble (preDigits, postDigits) x =
    preFilled ++ "." ++ postFilled ++ " - " ++ show x
  where
    preFilled = if length pre > preDigits then pre else
        reverse $ take preDigits $ (++ repeat ' ') $ reverse pre
    postFilled = take postDigits post
    pre = if x >= 0 then absPre else '-' : absPre
    absPre = show (truncate (abs x))
    post = postInner $ foldToRange (0, 1) (abs x)
    postInner rest = show (truncate (rest * 10)) ++ postInner (foldToRange (0, 1) (rest * 10))


-- * File stuff

-- | returns unhidden files (files, dirs)
getFiles :: FilePath -> IO ([FilePath], [FilePath])
getFiles path = do
    exists <- doesDirectoryExist path
    canonicalPath <- canonicalizePath path
    assertIO exists (canonicalPath ++ " does not exist")

    all <- sort <$> filter (\ f -> not ("." `isPrefixOf` f)) <$> getDirectoryContents path
    files <- filterM (doesFileExist . (path </>)) all
    dirs <- filterM (doesDirectoryExist . (path </>)) all
    return (files, dirs)


assertDirectoryExists :: FilePath -> IO ()
assertDirectoryExists path = do
    exists <- doesDirectoryExist path
    canonicalPath <- canonicalizePath path
    assertIO exists (canonicalPath ++ " is not a directory")








