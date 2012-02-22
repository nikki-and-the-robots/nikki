#!/usr/bin/env runghc

{-# language PackageImports #-}


import Prelude hiding (any)

import Data.Char
import Data.Maybe
import Data.Set
import Data.List (isInfixOf, isPrefixOf)
import Data.Foldable as Foldable (any, mapM_, forM_)

import Text.Parsec
import Text.Printf

import Control.Monad (when)
import Control.Applicative ((<$>))
import Control.Exception

import System.FilePath
import System.Directory
import System.Process
import System.Posix.Env

import Utils.Scripting


executables = "dist" </> "build"

deploymentDir = "nikki"

nikkiExe = executables </> "nikki" </> "nikki"
coreExe = executables </> "core" </> "core"


main = do
    setLibraryPath

    prepareDeploymentDir
    copy nikkiExe
    copy coreExe
    copy (".." </> "data")
    Foldable.mapM_ copy =<< getDynamicDependencies
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

-- | ensure that an empty deploymentDir exists
prepareDeploymentDir = do
    e <- doesDirectoryExist deploymentDir
    when e $
        removeDirectoryRecursive deploymentDir
    createDirectory deploymentDir

-- | return all dynamically linked dependencies for both executables
getDynamicDependencies :: IO (Set FilePath)
getDynamicDependencies = do
    restarterDeps <- getDeps nikkiExe
    coreDeps <- getDeps coreExe
    gnuTlsAssertion (union restarterDeps coreDeps)
    let allDeps = Data.Set.filter (not . isStandardLibrary) (union restarterDeps coreDeps)

    return allDeps
  where
    gnuTlsAssertion deps =
        assertLabel "linked to libcurl-gnutls: please link against libcurl instead"
            (noCurlGnuTlsLinked deps)
    noCurlGnuTlsLinked :: Set FilePath -> Bool
    noCurlGnuTlsLinked = not . any ("curl-gnutls" `isInfixOf`)

    assertLabel :: String -> Bool -> IO ()
    assertLabel msg False = error msg
    assertLabel _ True = return ()

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
    "libQtDBus" :
    "libQtXml" :

    -- other
    "libopenal" :
    "libzip" :
    "libaudio" :
    "libgmp" :
    "libpng" :
    "libcurl" :
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
