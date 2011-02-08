#!/usr/bin/env runhaskell

{-# language PackageImports #-}


import Data.Char
import Data.Maybe
import Data.Set

import "parsec3" Text.Parsec

import Control.Exception

import System.FilePath
import System.Directory
import System.Process

import Utils


executables = "dist" </> "build"

deploymentDir = "nikki"

nikkiExe = executables </> "nikki" </> "nikki"
coreExe = executables </> "core" </> "core"


main = do
    prepareDeploymentDir
    copy nikkiExe
    strip (deploymentDir </> takeBaseName nikkiExe)
    copy coreExe
    strip (deploymentDir </> takeBaseName coreExe)
    copy (".." </> "data")
    fmapM_ copy =<< getDynamicDependencies
    let deploymentIndicator = deploymentDir </> "yes_nikki_is_deployed"
    copyDeploymentLicenses
    putStrLn ("touching " ++ deploymentIndicator)
    writeFile deploymentIndicator ""

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
    forM_ restarterDeps $ \ l ->
        assertLabel ("not a standard library: " ++ l) (isStandardLibrary l)
    coreDeps <- getDeps coreExe
    let allDeps = Data.Set.filter (not . isStandardLibrary) coreDeps
    return allDeps
  where
    assertLabel :: String -> Bool -> IO ()
    assertLabel msg False = error msg
    assertLabel _ True = return ()

-- | Tries to guess, if a library will be present on a standard linux system.
-- The library is given with its full path on the current system.
isStandardLibrary :: FilePath -> Bool
isStandardLibrary s =
    not (libName `member` nonStandardLibraries)
  where
    libName = takeWhile (/= '.') $ takeFileName s

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
    [])


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

-- | strip an executable
strip :: FilePath -> IO ()
strip exe = do
    putStrLn ("stripping " ++ exe)
    trySystem ("strip " ++ exe)

-- * ldd output parsing

data LDDDep = LDDDep {dep :: FilePath, location :: Maybe FilePath}
  deriving Show

-- | return the dynamically linked dependencies for the given executables
getDeps :: FilePath -> IO (Set FilePath)
getDeps exe = do
    lddOutput <- readProcess "ldd" [exe] ""
    return $ case parse lddParser ("ldd-output: " ++ lddOutput) lddOutput of
        Left x -> error $ show x
        Right x -> filterWantedDeps x
  where
    -- filter for all the dependency we really want to deploy
    filterWantedDeps :: [LDDDep] -> (Set FilePath)
    filterWantedDeps = fromList . catMaybes . fmap convert
    convert :: LDDDep -> Maybe FilePath
    convert (LDDDep dep (Just location)) = Just location
    convert (LDDDep dep Nothing) = Nothing

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
