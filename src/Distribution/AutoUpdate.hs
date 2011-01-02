{-# language ViewPatterns #-}

-- | auto-updating for Nikki

module Distribution.AutoUpdate (autoUpdate) where


import Prelude hiding (catch)

import Data.Version
import Data.List

import Text.ParserCombinators.ReadP

import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.CatchIO

import System.Environment.FindBin
import System.FilePath
import System.Directory
import System.Info
import System.Exit
import System.IO.Temp (createTempDirectory)

import Version
import Utils

import Base.Monad
import Base.Configuration

import Distribution.AutoUpdate.Paths
import Distribution.AutoUpdate.Download
import Distribution.AutoUpdate.Zip


-- * introduced for more type safety

-- | Root of the directory where the game is deployed
data DeployPath = DeployPath FilePath

-- | where the new versions gets saved and unzipped after download
data ZipFilePath = ZipFilePath FilePath
data NewVersionDir = NewVersionDir FilePath


-- | If the application is actually deployed.
-- This is indicated by the existence of
-- 1. the core executable (as found with FindBin),
-- 2. the root of the deployed directory (whose path is platform dependent (see relativeDeployPath)
-- 3. and a (possibly empty) file called "deployed"
--    (residing in the root of the deployed directory).
isDeployed :: IO (Maybe DeployPath)
isDeployed = do
    progPath <- getProgPath
    let coreExecutable = progPath </> "core"
    coreExists <- doesFileExist coreExecutable
    let deployDirectory = progPath </> relativeDeployPath
    deployExists <- doesDirectoryExist deployDirectory
    hasDeployFile <- doesFileExist (deployDirectory </> "deployed")
    return $ if coreExists && deployExists && hasDeployFile then
        Just $ DeployPath deployDirectory
      else
        Nothing

-- | doing the auto update (wrapping the logic thread)
autoUpdate :: M a -> M a
autoUpdate game = do
    no_update_ <- asks no_update
    if no_update_ then do
        io $ putStrLn "not updating"
        game
      else
        doAutoUpdate game

doAutoUpdate :: M a -> M a
doAutoUpdate game = do
    repoString <- asks update_repo
    mDeployed <- io $ isDeployed
    case mDeployed of
        Nothing -> do
            io $ putStrLn "not deployed: not updating"
            game
        Just path -> do
            io $ putStrLn "deployed: looking for updates..."
            result <- io $ attemptUpdate (Repo repoString) path
            case result of
                (Left message) -> do
                    io $ putStrLn ("update failed: " ++ message)
                    game
                (Right True) -> do
                    io $ putStrLn "game updated"
                    io $ exitWith $ ExitFailure 143
                (Right False) -> do
                    io $ putStrLn ("version up to date")
                    game

-- | Looks for updates on the server.
-- If found, updates the program.
-- Returns (Right True) if an update was successfully installed,
-- (Right False) if there is no newer version and
-- (Left message) if an error occurs.
attemptUpdate :: Repo -> DeployPath -> IO (Either String Bool)
attemptUpdate repo deployPath = runErrorT $ do
    io $ putStrLn ("local version: " ++ showVersion Version.nikkiVersion)
    serverVersion <- parse =<< downloadContent (mkUrl repo "version")
    io $ putStrLn ("remote version: " ++ showVersion serverVersion)
    if serverVersion > Version.nikkiVersion then do
        update repo serverVersion deployPath
        return True
      else
        return False
  where
    parse :: Monad m => String -> ErrorT String m Version
    parse (stripWhiteSpaces -> s) = case readP_to_S parseVersion s of
        (last -> (v, "")) -> return v
        x -> throwError ("version parse error: " ++ show (s, x))

-- | the actual updating procedure
update :: Repo -> Version -> DeployPath -> ErrorT String IO ()
update repo newVersion deployPath = withSystemTempDirectory "nikki-update" $ \ downloadDir -> do
    zipFile <- downloadUpdate repo newVersion downloadDir
    newVersionDir <- unzipFile zipFile
    -- (withBackup creates its own temporary directory.)
    withBackup deployPath $
        installUpdate newVersionDir deployPath

-- | downloads the update to 
downloadUpdate :: Repo -> Version -> FilePath -> ErrorT String IO ZipFilePath
downloadUpdate repo newVersion tmpDir = do
    let zipFile = ("nikki-" ++ showVersion newVersion) <.> "zip"
    downloadFile (mkUrl repo zipFile) (tmpDir </> zipFile)
    return $ ZipFilePath (tmpDir </> zipFile)

-- | unzips a given zipFile (in the same directory) and returns the path to the unzipped directory
unzipFile :: ZipFilePath -> ErrorT String IO NewVersionDir
unzipFile (ZipFilePath path) = do
    io $ unzipArchive path (takeDirectory path)
    let nikkiDir = takeDirectory path </> "nikki"
    nikkiExists <- io $ doesDirectoryExist nikkiDir
    when (not nikkiExists) $ throwError ("directory not found: " ++ nikkiDir)
    return $ NewVersionDir nikkiDir

-- | Backups all files to a temporary directory.
-- Restores them in case anything goes wrong.
-- Deletes the backup in case of a successful action (except the executables).
-- Catches every exception and every ErrorT error.
-- In any case: the executables don't get deleted.
withBackup :: DeployPath -> ErrorT String IO a -> ErrorT String IO a
withBackup (DeployPath deployPath) action = do
  deployedFiles <- sort <$> filter isContentFile <$> io (getDirectoryContents deployPath)
  withTempDirectory deployPath "backup" $ \ tmpDir -> do

    let backup :: ErrorT String IO ()
        backup = do
            forM_ deployedFiles $ \ f ->
                rename (deployPath </> f) (tmpDir </> f)
        restore :: ErrorT String IO ()
        restore = do
            io $ putStrLn "restoring"
            forM_ deployedFiles $ \ f -> do
                let dest = deployPath </> f
                removeIfExists dest
                rename (tmpDir </> f) dest
        conserveExecutables :: ErrorT String IO ()
        conserveExecutables = io $ do
            -- create a temp dir for the executables
            executableTempDirectory <- createTempDirectory deployPath "temporaryExecutables"
            -- move the executables to the new temp dir
            let backupedExecutableDir = tmpDir </> deployRootToExecutables
            renameFile (backupedExecutableDir </> restarterExecutable)
                       (executableTempDirectory </> restarterExecutable)
            renameFile (backupedExecutableDir </> coreExecutable)
                       (executableTempDirectory </> coreExecutable)

    backup
    result <- catchError
                (action `onException` restore)
                (\ errorMessage -> restore >> throwError errorMessage)
    conserveExecutables
    return result
  where

    isContentFile "." = False
    isContentFile ".." = False
    isContentFile _ = True

    -- | renaming directories and files
    rename src dest = do
        io $ putStrLn ("renaming: " ++ src ++ " -> " ++ dest)
        isFile <- io $ doesFileExist src
        isDirectory <- io $ doesDirectoryExist src
        if isFile then
            io $ renameFile src dest
          else if isDirectory then
            io $ renameDirectory src dest
          else
            throwError ("file not found: " ++ src)

-- | installs the update
installUpdate :: NewVersionDir -> DeployPath -> ErrorT String IO ()
installUpdate (NewVersionDir newVersionDir) (DeployPath deployPath) = io $ do
    copyDirectory newVersionDir deployPath
    -- adding executable rights to the executables
    forM_ [restarterExecutable, coreExecutable] $ \ exe -> do
        let p = deployPath </> deployRootToExecutables </> exe
        perm <- getPermissions p
        setPermissions p perm{executable = True}


-- * temp functions

-- reimplementations of functions from System.IO.Temp with a more polymorphic type.

withSystemTempDirectory :: MonadCatchIO m => String -> (FilePath -> m a) -> m a
withSystemTempDirectory template action =
    io getTemporaryDirectory >>= \tmpDir -> withTempDirectory tmpDir template action

withTempDirectory :: MonadCatchIO m => FilePath -> String -> (FilePath -> m a) -> m a
withTempDirectory targetDir template =
  bracket
    (io $ createTempDirectory targetDir template)
    (io . removeDirectoryRecursive)
