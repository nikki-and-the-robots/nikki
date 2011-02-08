{-# language EmptyDataDecls, FlexibleInstances, ViewPatterns #-}

module Utils (
    (<$>),
    (<*>),
    (>>>),
    trace,
    forM,
    forM_,
    when,
    module Utils,
  ) where

-- imports

import Prelude hiding (catch)
import qualified Prelude

import Safe

import Data.List
import Data.Map (Map, fromList, member, (!), findWithDefault, toList)
import Data.Foldable (Foldable, mapM_, forM_)
import Data.Traversable (Traversable, mapM)
import Data.IORef
import qualified Data.Set as Set
import Data.Char

import Text.Printf
import Text.Logging

import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Monad.State hiding (forM_)
import Control.Monad.Trans.Error () -- Monad (Either e)
import Control.Arrow ((>>>))
import Control.Concurrent

import System.Directory
import System.IO.Unsafe
import System.FilePath
import System.Cmd
import System.Exit

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

traceThis :: String -> (x -> String) -> x -> x
traceThis msg showFun x = trace (msg ++ ": " ++ showFun x) x



e :: String -> a
e = error

es :: Show s => String -> s -> a
es msg x = error (msg ++ ": " ++ show x)

nm :: Show s => String -> s -> a
nm msg = es ("Non-exhaustive patterns: " ++ msg)


{-# NOINLINE debug #-}
debug :: String -> a -> a
debug msg x = unsafePerformIO $ do
    logInfo ("DEBUG: " ++ msg)
    return x

debugs :: Show s => String -> s -> a -> a
debugs msg s = debug (msg ++ ": " ++ show s)

printDebug :: String -> IO ()
printDebug msg = logInfo ("\tDEBUG: " ++ msg)

assertIO :: Bool -> String -> IO ()
assertIO True _ = return ()
assertIO False msg = error ("ASSERTION ERROR: " ++ msg)

warn :: MonadIO m => String -> m ()
warn m = io $ logInfo ("WARNING: " ++ m)

toDebug :: Show s => String -> s -> String
toDebug msg s = msg ++ ": " ++ show s

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


-- * re-named re-exports

fmapM :: (Data.Traversable.Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
fmapM = Data.Traversable.mapM

fmapM_ :: (Monad m, Data.Foldable.Foldable t) => (a -> m b) -> t a -> m ()
fmapM_ = Data.Foldable.mapM_


-- * function composition stuff

(|>) :: a -> (a -> b) -> b
a |> f = f a

-- fake kleisli stuff

(>>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
x >>>> y = \ a -> x a >>= y

passThrough :: Monad m => (a -> m ()) -> (a -> m a)
passThrough cmd a = cmd a >> return a

fromPure :: Monad m => (a -> b) -> (a -> m b)
fromPure = (return .)

secondKleisli :: Monad m => (a -> m b) -> ((x, a) -> m (x, b))
secondKleisli cmd (x, a) = do
    b <- cmd a
    return (x, b)


-- * scripting stuff

-- | executes a unix command on the shell and exits if it does not succeed.
trySystem :: String -> IO ()
trySystem cmd = do
    logInfo ("Executing \"" ++ cmd ++ "\" ...")
    exitcode <- system cmd
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure n -> exitWith $ ExitFailure n

-- | changes the working directory temporarily.
withCurrentDirectory :: FilePath -> IO () -> IO ()
withCurrentDirectory path cmd = do
    logInfo ("Entering directory " ++ path)
    oldWorkingDirectory <- getCurrentDirectory
    setCurrentDirectory path
    x <- cmd
    setCurrentDirectory oldWorkingDirectory
    logInfo ("Leaving directory " ++ path)
    return x

-- | copy a whole directory recursively
-- excluding hidden files
-- give full paths to both directories, e.g. (copyDirectory "src/dir" "dest/dir")
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
    allFiles <- getFilesRecursive src
    forM_ allFiles copy
  where
    copy file = do
        createDirectoryIfMissing True (takeDirectory (dst </> file))
        copyFile (src </> file) (dst </> file)

-- | returns all (unhidden) files in a directory recursively, sorted.
-- Omits the directories.
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive dir =
    map normalise <$> inner dir "."
  where
    inner root dir = do
        content <- map (dir </>) <$> sort <$> getFiles (root </> dir) Nothing
        (directories, files) <- partitionM (doesDirectoryExist . (root </>)) content
        recursive <- fmapM (inner root) $ directories
        return $ sort (files ++ concat recursive)

-- | removes file and directories if they exist
removeIfExists f = io $ do
    isFile <- doesFileExist f
    isDirectory <- doesDirectoryExist f
    when (isFile || isDirectory) $
        logInfo ("removing: " ++ f)
    if isFile then
        removeFile f
      else if isDirectory then
        removeDirectoryRecursive f
      else
        return ()

-- | Returns all files and directories in a given directory, sorted.
-- Omit "." and "..".
getDirectoryRealContents :: FilePath -> IO [FilePath]
getDirectoryRealContents path =
    io $ sort <$> filter isContent <$> getDirectoryContents path
  where
    isContent "." = False
    isContent ".." = False
    isContent _ = True

-- | Returns if a path starts with a dot.
isHiddenOnUnix :: FilePath -> Bool
isHiddenOnUnix = headMay >>> (== Just '.')

-- | Returns all unhidden (unix) files in a given directory.
-- @getFiles dir (Just extension)@ returns all files with the given extension.
getFiles :: FilePath -> Maybe String -> IO [FilePath]
getFiles dir mExtension =
    sort <$> filter hasRightExtension <$> filter (not . isHiddenOnUnix) <$>
        getDirectoryRealContents dir
  where
    hasRightExtension :: FilePath -> Bool
    hasRightExtension = case mExtension of
        (Just ('.' : '.' : r)) -> error ("don't give extensions that start with two dots: " ++ r)
        (Just extension@('.' : _)) -> takeExtension >>> (== extension)
        (Just extension) -> takeExtension >>> (== ('.' : extension))
        Nothing -> const True


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


-- * Monad stuff

chainAppM :: Monad m => (b -> a -> m a) -> [b] -> a -> m a
chainAppM cmd (b : r) a = do
    a' <- cmd b a
    chainAppM cmd r a'
chainAppM _ [] a = return a

void :: Monad m => m a -> m ()
void = (>> return ())

io :: MonadIO m => IO a -> m a
io = liftIO

partitionM :: (a -> IO Bool) -> [a] -> IO ([a], [a])
partitionM p (a : r) = do
    condition <- p a
    (yes, no) <- partitionM p r
    return $ if condition then
        (a : yes, no)
      else
        (yes, a : no)
partitionM _ [] = return ([], [])


-- * list stuff

infixl 4 +:
(+:) :: [a] -> a -> [a]
a +: b = a ++ [b]

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
cartesian a b = do
    a' <- a
    b' <- b
    return (a', b')

-- | returns every combination of given elements once.
completeEdges :: [a] -> [(a, a)]
completeEdges (a : r) = map (tuple a) r ++ completeEdges r
completeEdges [] = []

adjacentCyclic :: [a] -> [(a, a)]
adjacentCyclic [] = []
adjacentCyclic [a] = []
adjacentCyclic list@(head : _) = inner head list
  where
    inner first (a : b : r) = (a, b) : inner first (b : r)
    inner first [last] = [(last, first)]

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
    inner a [] = Nothing

-- | like mergePairs, but only tries to merge adjacent elements (or the first and the last element)
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

stripWhiteSpaces :: String -> String
stripWhiteSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse


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

mapPairs :: Ord k => (k -> a -> (k, a)) -> Map k a -> Map k a
mapPairs f = fromList . map (uncurry f) . toList


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

justWhen :: Bool -> a -> Maybe a
justWhen True = Just
justWhen False = const Nothing


-- * math stuff

rad2deg :: Floating a => a -> a
rad2deg x = (x * 360) / (pi * 2)

deg2rad :: Floating a => a -> a
deg2rad x = x * 2 * pi / 360

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

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

third :: (a, b, c) -> c
third (a, b, c) = c


-- * misc

readE :: Read r => String -> r
readE r = case readM r of
    Right x -> x
    Left msg -> error msg

readM :: (Monad m, Read r) => String -> m r
readM x = unsafePerformIO $
    Prelude.catch
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

-- | boolean implication
infix 4 ~>
(~>) :: Bool -> Bool -> Bool
True ~> x = x
False ~> _ = True

-- | reads all currently available messages from the channel.
pollChannel :: Chan a -> IO [a]
pollChannel chan = do
    empty <- isEmptyChan chan
    if empty then
        return []
      else do
        a <- readChan chan
        r <- pollChannel chan
        return (a : r)

fixpoint :: Eq e => (e -> e) -> e -> e
fixpoint f x = if fx == x then x else fixpoint f fx
  where
    fx = f x

-- | applies a function n times
superApply :: Int -> (a -> a) -> a -> a
superApply n f = foldr (.) id $ replicate n f


-- * Pretty Printing

class PP a where
    pp :: a -> String

instance (PP a, PP b) => PP (a, b) where
    pp (a, b) = "(" ++ pp a ++ ", " ++ pp b ++ ")"

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


instance PP Double where
    pp = printf "%8.3f"

instance PP Int where
    pp = show

ppp :: PP p => p -> IO ()
ppp = pp >>> logInfo

