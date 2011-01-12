{-# language ViewPatterns, ScopedTypeVariables #-}

-- | auto-updating for Nikki

module Distribution.AutoUpdate (autoUpdate) where


import Prelude hiding (catch)

import Data.List

import Text.ParserCombinators.ReadP

import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.CatchIO
import Control.Concurrent

import System.Environment.FindBin
import System.FilePath
import System.Directory
import System.Info
import System.Exit
import System.IO.Temp (createTempDirectory)

import Version
import Utils

import Base.Types
import Base.Monad
import Base.Configuration
import Base.Application
import Base.Application.GUILog

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
-- 3. and a (possibly empty) file called "yes_nikki_is_deployed"
--    (residing in the root of the deployed directory).
isDeployed :: IO (Maybe DeployPath)
isDeployed = do
    progPath <- getProgPath
    let coreExecutable = progPath </> mkExecutable "core"
    coreExists <- doesFileExist coreExecutable
    deployDirectory <- canonicalizePath (progPath </> relativeDeployPath)
    deployExists <- doesDirectoryExist deployDirectory
    hasDeployFile <- doesFileExist (deployDirectory </> "yes_nikki_is_deployed")
    return $ if coreExists && deployExists && hasDeployFile then
        Just $ DeployPath deployDirectory
      else
        Nothing

-- | doing the auto update (wrapping the logic thread)
autoUpdate :: Application_ sort -> M a -> M a
autoUpdate app game = do
    no_update_ <- gets no_update
    if no_update_ then do
        io $ guiLog app "not updating"
        game
      else
        doAutoUpdate app game

doAutoUpdate :: Application_ sort -> M a -> M a
doAutoUpdate app game = do
    repoString <- gets update_repo
    mDeployed <- io $ isDeployed
    case mDeployed of
        Nothing -> do
            io $ guiLog app "not deployed: not updating"
            game
        Just path@(DeployPath dp) -> do
            io $ guiLog app ("deployed in " ++ dp ++ "\nlooking for updates...")
            result <- io $ attemptUpdate app (Repo repoString) path
            case result of
                (Left message) -> do
                    io $ guiLog app ("update failed: " ++ message)
                    game
                (Right True) -> io $ do
                    guiLog app "game updated...\nrestarting..."
                    threadDelay 5000000
                    exitWith $ ExitFailure 143
                (Right False) -> do
                    io $ guiLog app ("version up to date")
                    game

-- | Looks for updates on the server.
-- If found, updates the program.
-- Returns (Right True) if an update was successfully installed,
-- (Right False) if there is no newer version and
-- (Left message) if an error occurs.
attemptUpdate :: Application_ sort -> Repo -> DeployPath -> IO (Either String Bool)
attemptUpdate app repo deployPath = runErrorT $ do
    io $ guiLog app ("local version: " ++ showVersion Version.nikkiVersion)
    serverVersion :: Version <-
        (ErrorT . return . parseVersion) =<< downloadContent (mkUrl repo "version")
    io $ guiLog app ("remote version: " ++ showVersion serverVersion)
    if serverVersion > Version.nikkiVersion then do
        update app repo serverVersion deployPath
        return True
      else
        return False

-- | the actual updating procedure
update :: Application_ sort -> Repo -> Version -> DeployPath -> ErrorT String IO ()
update app repo newVersion deployPath = withSystemTempDirectory "nikki-update" $ \ downloadDir -> do
    zipFile <- downloadUpdate app repo newVersion downloadDir
    newVersionDir <- unzipFile app zipFile
    -- (withBackup creates its own temporary directory.)
    withBackup app deployPath $
        installUpdate newVersionDir deployPath

-- | downloads the update to 
downloadUpdate :: Application_ sort -> Repo -> Version -> FilePath -> ErrorT String IO ZipFilePath
downloadUpdate app repo newVersion tmpDir = do
    let zipFile = ("nikki-" ++ showVersion newVersion) <.> "zip"
    downloadFile app (mkUrl repo zipFile) (tmpDir </> zipFile)
    return $ ZipFilePath (tmpDir </> zipFile)

-- | unzips a given zipFile (in the same directory) and returns the path to the unzipped directory
unzipFile :: Application_ sort -> ZipFilePath -> ErrorT String IO NewVersionDir
unzipFile app (ZipFilePath path) = do
    io $ unzipArchive (guiLog app) path (takeDirectory path)
    let nikkiDir = takeDirectory path </> mkDeployedFolder "nikki"
    nikkiExists <- io $ doesDirectoryExist nikkiDir
    when (not nikkiExists) $ throwError ("directory not found: " ++ nikkiDir)
    return $ NewVersionDir nikkiDir

-- | Backups all files to a temporary directory.
-- Restores them in case anything goes wrong.
-- Catches every exception and every ErrorT error.
-- Leaves the backup where it is (in a folder called "temporaryBackupSOMETHING",
-- which will be deleted by the restarter at a later launch.)
withBackup :: Application_ sort -> DeployPath -> ErrorT String IO a -> ErrorT String IO a
withBackup app (DeployPath deployPath) action = do
    deployedFiles <- io $ sort <$> getDirectoryRealContents deployPath
    tmpDir <- io $ createTempDirectory deployPath "temporaryBackup"

    let backup :: ErrorT String IO ()
        backup = do
            forM_ deployedFiles $ \ f ->
                rename (deployPath </> f) (tmpDir </> f)
        restore :: ErrorT String IO ()
        restore = do
            io $ guiLog app "restoring"
            forM_ deployedFiles $ \ f -> do
                let dest = deployPath </> f
                removeIfExists dest
                rename (tmpDir </> f) dest

    backup
    result <- catchError
                (action `onException` restore)
                (\ errorMessage -> restore >> throwError errorMessage)
    return result
  where

    -- | renaming directories and files
    rename src dest = do
        io $ guiLog app ("renaming: " ++ src ++ " -> " ++ dest)
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
