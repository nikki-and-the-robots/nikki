{-# language ViewPatterns, ScopedTypeVariables #-}

-- | auto-updating for Nikki

module Distribution.AutoUpdate (autoUpdate) where


import Prelude hiding (catch)

import Data.List
import Data.Monoid

import Control.Monad.Trans.Error
import Control.Monad.CatchIO

import System.Environment.FindBin
import System.FilePath
import System.Directory
import System.Exit
import System.IO.Temp (createTempDirectory)
import qualified System.Info

import Version
import Utils

import Base.Types hiding (update)
import Base.Prose
import Base.Monad
import Base.Configuration
import Base.Renderable.GUILog
import Base.Renderable.Message

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

-- | doing the auto update
autoUpdate :: Application_ sort -> AppState -> AppState
autoUpdate app follower = NoGUIAppState $ io $ do
    (renderable, logCommand) <- mkGuiLog app
    return $ AppState renderable $ do
        repoString <- gets update_repo
        mDeployed <- io $ isDeployed
        case mDeployed of
            Nothing -> return $ message app [p "not deployed: not updating"] follower
            Just path@(DeployPath dp) -> do
                io $ do
                    logCommand (p "deployed in " `mappend` pVerbatim dp)
                    logCommand (p "looking for updates...")
                result <- io $ attemptUpdate app logCommand (Repo repoString) path
                case result of
                    (Left errorMessage) ->
                        return $ message app [p "update failed: " +> pVerbatim errorMessage] follower
                    (Right (Just version)) -> do
                        return $ message app
                            (p "game updated to version " +> pVerbatim (showVersion version) :
                             p "restarting..." :
                             []) $ NoGUIAppState $ io $ do
                                exitWith $ ExitFailure 143
                    (Right Nothing) ->
                        return $ message app [p "version up to date"] follower

-- | Looks for updates on the server.
-- If found, updates the program.
-- Returns (Right (Just newVersion)) if an update was successfully installed,
-- (Right Nothing) if there is no newer version and
-- (Left message) if an error occurs.
attemptUpdate :: Application_ sort -> (Prose -> IO ()) -> Repo -> DeployPath
    -> IO (Either String (Maybe Version))
attemptUpdate app logCommand repo deployPath = runErrorT $ do
    io $ logCommand (p "local version: " `mappend` pVerbatim (showVersion Version.nikkiVersion))
    serverVersion :: Version <-
        (ErrorT . return . parseVersion) =<< downloadContent (mkUrl repo "version")
    io $ logCommand (p "remote version: " `mappend` pVerbatim (showVersion serverVersion))
    if serverVersion > Version.nikkiVersion then do
        update app logCommand repo serverVersion deployPath
        return $ Just serverVersion
      else
        return Nothing

-- | the actual updating procedure
update :: Application_ sort -> (Prose -> IO ()) -> Repo -> Version -> DeployPath
    -> ErrorT String IO ()
update app logCommand repo newVersion deployPath = withSystemTempDirectory "nikki-update" $ \ downloadDir -> do
    zipFile <- downloadUpdate app logCommand repo newVersion downloadDir
    newVersionDir <- unzipFile app logCommand zipFile
    -- (withBackup creates its own temporary directory.)
    withBackup app logCommand deployPath $
        installUpdate newVersionDir deployPath

-- | downloads the update to 
downloadUpdate :: Application_ sort -> (Prose -> IO ()) -> Repo -> Version -> FilePath
    -> ErrorT String IO ZipFilePath
downloadUpdate app logCommand repo newVersion tmpDir = do
    let zipFile = ("nikki-" ++ showVersion newVersion) <.> "zip"
    downloadFile app logCommand (mkUrl repo zipFile) (tmpDir </> zipFile)
    return $ ZipFilePath (tmpDir </> zipFile)

-- | unzips a given zipFile (in the same directory) and returns the path to the unzipped directory
unzipFile :: Application_ sort -> (Prose -> IO ()) -> ZipFilePath
    -> ErrorT String IO NewVersionDir
unzipFile app logCommand (ZipFilePath path) = do
    io $ logCommand (p "unzipping " `mappend` pVerbatim path)
    io $ unzipArchive path (takeDirectory path)
    let nikkiDir = takeDirectory path </> mkDeployedFolder "nikki"
    nikkiExists <- io $ doesDirectoryExist nikkiDir
    when (not nikkiExists) $ throwError ("directory not found: " ++ nikkiDir)
    return $ NewVersionDir nikkiDir

-- | Backups all files to a temporary directory.
-- Restores them in case anything goes wrong.
-- Catches every exception and every ErrorT error.
-- Leaves the backup where it is (in a folder called "temporaryBackupSOMETHING",
-- which will be deleted by the restarter at a later launch.)
withBackup :: Application_ sort -> (Prose -> IO ()) -> DeployPath
    -> ErrorT String IO a -> ErrorT String IO a
withBackup app logCommand (DeployPath deployPath) action = do
    deployedFiles <- io $ sort <$> getDirectoryRealContents deployPath
    tmpDir <- io $ createTempDirectory deployPath "temporaryBackup"

    let backup :: ErrorT String IO ()
        backup = do
            forM_ deployedFiles $ \ f ->
                rename (deployPath </> f) (tmpDir </> f)
        restore :: ErrorT String IO ()
        restore = do
            io $ logCommand (p "restoring backup")
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
    forM_ executables $ \ exe -> do
        let p = deployPath </> deployRootToExecutables </> exe
        perm <- getPermissions p
        setPermissions p perm{executable = True}
  where
    executables = case System.Info.os of
        "linux" -> [linuxStartScript, linuxRenamedRestarter, coreExecutable]
        _ -> [restarterExecutable, coreExecutable]


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
