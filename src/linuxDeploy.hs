#!/usr/bin/env stack
{- stack script --resolver=lts-10.2
--package safe
--package directory
--package FindBin
--package process
--package filepath
--package containers
--package parsec
--package unix
--package getopt-generics
-}

{-# LANGUAGE DeriveGeneric #-}


import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad (when)
import Data.Char
import Data.Foldable as Foldable (any, mapM_, forM_)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe
import Data.Set
import Prelude hiding (any)
import System.Directory
import System.FilePath
import System.Posix.Env
import System.Process
import Text.Parsec
import Text.Printf
import Utils.Scripting
import WithCli


executablesDir distDir = distDir </> "build"

deploymentDir = "nikki"

nikkiExe distDir =
  executablesDir distDir </> "nikki" </> "nikki"


data Args = Args {
  noCopySharedObjects :: Bool
} deriving (Generic)

instance HasArguments Args

main :: IO ()
main = withCli $ \ args -> do
    when (not (noCopySharedObjects args)) $ do
      setLibraryPath

    prepareDeploymentDir
    distDir <- getDistDir
    copy $ nikkiExe distDir
    copy (".." </> "data")
    when (not (noCopySharedObjects args)) $ do
      Foldable.mapM_ copy =<< getDynamicDependencies distDir
    let deploymentIndicator = deploymentDir </> "yes_nikki_is_deployed"
    copyDeploymentLicenses
    fiddleInStartScript
    putStrLn ("touching " ++ deploymentIndicator)
    writeFile deploymentIndicator ""

setLibraryPath :: IO ()
setLibraryPath = do
    v <- getEnv "LD_LIBRARY_PATH"
    maybe (return ()) (const $ error "LD_LIBRARY_PATH already set") v
    setEnv "LD_LIBRARY_PATH" "/usr/local/lib" False

getDistDir :: IO FilePath
getDistDir = do
  strip <$> readProcess "stack" ["path", "--dist-dir"] ""

-- | ensure that an empty deploymentDir exists
prepareDeploymentDir = do
    e <- doesDirectoryExist deploymentDir
    when e $
        removeDirectoryRecursive deploymentDir
    createDirectory deploymentDir

-- | return all dynamically linked dependencies for both executables
getDynamicDependencies :: FilePath -> IO (Set FilePath)
getDynamicDependencies distDir = do
    nikkiDeps <- getDeps $ nikkiExe distDir
    let allDeps = Data.Set.filter (not . isStandardLibrary) nikkiDeps
    return allDeps


-- | Tries to guess, if a library will be present on a standard linux system.
-- The library is given with its full path on the current system.
isStandardLibrary :: FilePath -> Bool
isStandardLibrary s =
    not (any (`isPrefixOf` takeFileName s) nonStandardLibraries)

-- | Set of libraries expected not to be on every standard linux system.
nonStandardLibraries :: Set String
nonStandardLibraries = fromList (
    -- Qt
    "libQtOpenGL" :
    "libQtCore" :
    "libQtGui" :

    -- other
    "libopenal" :
    "libzip" :
    "libaudio" :
    "libgmp" :
    "libpng" :

    [])

-- | list of unwanted dependencies
unwantedDependencies :: [String]
unwantedDependencies =
    "libssl" :
    "libcrypto" :
    "librtmp" :
    []

-- | copy the licenses to the deploymentDir
copyDeploymentLicenses :: IO ()
copyDeploymentLicenses = do
    let licenseDir = ".." </> "deploymentLicenses"
    files <- fmap (licenseDir </>) <$> getFiles licenseDir Nothing
    forM_ files copy

-- | copy the given file to the deploymentDir
copy :: FilePath -> IO ()
copy path = do
    putStrLn ("copying " ++ path)
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    if isFile then
        copyFile path (deploymentDir </> takeFileName path)
      else if isDir then
        copyDirectory path (deploymentDir </> takeFileName path)
      else
        error ("not found: " ++ path)

-- * ldd output parsing

data LDDDep = LDDDep {dep :: FilePath, location :: Maybe FilePath}
  deriving Show

-- | return the dynamically linked dependencies for the given executables
getDeps :: FilePath -> IO (Set FilePath)
getDeps exe = do
    lddOutput <- readProcess "ldd" [exe] ""
    let deps = case parse lddParser ("ldd-output: " ++ lddOutput) lddOutput of
            Left x -> error $ show x
            Right x -> filterWantedDeps x
    assertWantedDeps deps
    return deps
  where
    -- filter for all the dependency we really want to deploy
    filterWantedDeps :: [LDDDep] -> Set FilePath
    filterWantedDeps = fromList . catMaybes . fmap convert
    convert :: LDDDep -> Maybe FilePath
    convert (LDDDep dep (Just location)) = Just location
    convert (LDDDep dep Nothing) = Nothing

    -- asserts that nikki doesn't depend on libssl, libcrypto or librtmp
    assertWantedDeps :: Set FilePath -> IO ()
    assertWantedDeps deps =
        forM_ deps $ \ dep ->
            when (isUnwantedDep dep) $
                error (printf "executable %s depends on unwanted library %s"
                    exe dep)
    isUnwantedDep :: FilePath -> Bool
    isUnwantedDep lib =
        any
        (\ unwanted -> unwanted `isPrefixOf`  takeBaseName lib)
        unwantedDependencies

lddParser :: Parsec String () [LDDDep]
lddParser = do
    r <- endBy dep newline
    eof
    return r
  where
    dep = spaces >> (absoluteDep <|> relativeDep)

    absoluteDep = do
        lookAhead (char '/')
        path <- token
        hex
        return $ LDDDep path Nothing

    relativeDep = do
        lookAhead (noneOf ['/'])
        path <- token
        spaces
        string "=>"
        spaces
        loc <- location
        hex
        return $ LDDDep path loc

    location :: Parsec String () (Maybe FilePath)
    location = optionMaybe $ do
        lookAhead $ noneOf ['(']
        token

    -- parses the hex number at the end of each entry
    hex = do
        spaces
        char '('
        many1 alphaNum
        char ')'

    -- parses any string till the next whitespace character
    token :: Parsec String () String
    token = many1 (satisfy (not . isSpace))


-- * start script stuff
-- We have to provide a bash start-script to set LD_LIBRARY_PATH
-- (We would use a haskell program for this, but haskell's RTS depends
-- on some libraries that might not be there (e.g. libgmp))

fiddleInStartScript :: IO ()
fiddleInStartScript = do
    putStrLn "fiddling in the start bash script"
    renameNikkiExe
    copyStartScript

-- We rename the restarter "nikki" to "restarter"
renameNikkiExe :: IO ()
renameNikkiExe =
    renameFile src dest
  where
    src = deploymentDir </> "nikki"
    dest = deploymentDir </> "restarter"

-- | copies the starting script to the deployed directory
copyStartScript :: IO ()
copyStartScript = do
    copyFile src dest
    _ <- system ("chmod +x " ++ dest)
    return ()
  where
    name = "nikki.sh"
    src = "bash" </> name
    dest = deploymentDir </> name

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
