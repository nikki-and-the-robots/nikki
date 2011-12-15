{-# language CPP, ViewPatterns, ScopedTypeVariables #-}

-- | auto-updating for Nikki

module Distribution.AutoUpdate (
    autoUpdate,
    getUpdateVersion,
    Repo(..),
  ) where


import Prelude hiding (catch)

import Data.List
import Data.Monoid
import Data.Version (Version, showVersion)
import Data.Maybe

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
import Base.Prose.Template
import Base.Monad
import Base.Configuration
import Base.Renderable.GUILog
import Base.Renderable.Message
import Base.Renderable.Menu
import Base.Renderable.OpenUrl

import Distribution.AutoUpdate.Paths
import Distribution.AutoUpdate.Download
import Distribution.AutoUpdate.Zip
import Distribution.AutoUpdate.VerifySignatures

import qualified StoryMode.Client
import qualified StoryMode.AutoUpdate


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

-- | gracefully fail, if the game is compiled to be installed as root.
autoUpdateRootInstall :: Application -> AppState -> AppState
autoUpdateRootInstall app follower = NoGUIAppState $ do
    repo <- Repo <$> gets update_repo
    v <- io $ runErrorT $ getUpdateVersion repo
    case v of
        Left errors -> return $ message app (map pv errors) follower
        Right (UpdateVersions Nothing Nothing) ->
            -- no new version available
            return $ message app [p "no updates available"] follower
        Right (UpdateVersions (Just gameNewVersion) _) -> do
            let typ newVersion = NormalMenu (pv "new version") (Just $
                        substitute [("newVersion", showVersion newVersion)] $ p
                                ("A new version of Nikki and the Robots is available: $newVersion! " ++
                                "Try using the software manager to get it or download it manually!"))
            return $ menuAppState app (typ gameNewVersion) (Just follower) (
                (p "Download manually (opens browser)",
                    const $ openUrl app downloadWebsite follower) :
                []) 0
        Right uvs@(UpdateVersions Nothing (Just storyModeNewVersion)) ->
          return $ guiLog app $ \ logCommand -> io $ do
            -- update the story mode (although installed as root)
            result :: Either [String] () <- runErrorT $
                                            StoryMode.AutoUpdate.update app logCommand
            case result of
                Left errorMessages ->
                    return $ message app (map pv errorMessages) follower
                Right () -> do
                    return $ message app
                        (p "update complete" :
                         updateSuccessMessage uvs ++
                         p "restarting..." :
                         []) $ NoGUIAppState $ io $ do
                            exitWith $ ExitFailure 143

-- | doing the auto update
autoUpdate :: Application -> AppState -> AppState
#ifdef RootInstall
autoUpdate app follower = autoUpdateRootInstall app follower
#else
autoUpdate app follower = guiLog app $ \ logCommand -> do
    repoString <- gets update_repo
    mDeployed <- io $ isDeployed
    case mDeployed of
        Nothing -> return $ message app [p "not deployed: updating disabled"] follower
        Just path@(DeployPath dp) -> do
            io $ logCommand (p "updating...")
            result <- io $ attemptUpdate app logCommand (Repo repoString) path
            case result of
                (Left errorMessages) ->
                    return $ message app (map pv errorMessages) follower
                (Right (UpdateVersions Nothing Nothing)) ->
                    return $ message app [p "no updates available"] follower
                (Right uvs) -> do
                    return $ message app
                        (p "update complete" :
                         updateSuccessMessage uvs ++
                         p "restarting..." :
                         []) $ NoGUIAppState $ io $ do
                            exitWith $ ExitFailure 143
-- RootInstall
#endif

updateSuccessMessage :: UpdateVersions -> [Prose]
updateSuccessMessage (UpdateVersions mGame mStoryMode) =
    catMaybes
    (fmap g mGame :
    fmap sm mStoryMode :
    [])
  where
    g v = p "new game version: " +> pVerbatim (showVersion v)
    sm v = p "new storymode version: " +> pVerbatim (showVersion v)


-- | Looks for updates on the server.
-- If found, updates the program.
-- Returns (Right newVersions) if an update was successfully installed,
-- (Right (UpdateVersion Nothing Nothing)) if there is no newer version and
-- (Left message) if an error occurs.
attemptUpdate :: Application -> (Prose -> IO ()) -> Repo -> DeployPath
    -> IO (Either [String] UpdateVersions)
attemptUpdate app logCommand repo deployPath = runErrorT $ do
    UpdateVersions mGameVersion mStoryModeVersion <- getUpdateVersion repo
    whenMaybe mGameVersion $ \ serverVersion -> do
        update app logCommand repo serverVersion deployPath
    whenMaybe mStoryModeVersion $ \ storyModeVersion -> do
        StoryMode.AutoUpdate.update app logCommand
    return $ UpdateVersions mGameVersion mStoryModeVersion

-- | Returns (Just newVersion), if a newer version is available from the update server.
getUpdateVersion :: Repo -> ErrorT [String] IO UpdateVersions
getUpdateVersion repo = catchSomeExceptionsErrorT (singleton . show) $ do
    serverVersion <- (ErrorT . return . parseVersion) =<< downloadContent (mkUrl repo "version")
    let gameNewVersion = if serverVersion > Version.nikkiVersion
            then Just serverVersion
            else Nothing
    storyModeNewVersion <- StoryMode.Client.askForNewVersion
    return $ UpdateVersions gameNewVersion storyModeNewVersion

-- | the actual updating procedure
update :: Application -> (Prose -> IO ()) -> Repo -> Version -> DeployPath
    -> ErrorT [String] IO ()
update app logCommand repo newVersion deployPath =
  catchSomeExceptionsErrorT (singleton . show) $
  withSystemTempDirectory "nikki-update" $ \ downloadDir -> do
    zipFile <- downloadUpdate app logCommand repo newVersion downloadDir
    newVersionDir <- unzipFile app logCommand zipFile
    -- (withBackup creates its own temporary directory.)
    withBackup app logCommand deployPath $
        installUpdate newVersionDir deployPath

-- | downloads the update.
-- Also downloads a signature and verifies the downloaded update against that signature.
downloadUpdate :: Application -> (Prose -> IO ()) -> Repo -> Version -> FilePath
    -> ErrorT [String] IO ZipFilePath
downloadUpdate app logCommand repo newVersion tmpDir = do
    let zipFile = ("nikki-" ++ showVersion newVersion) <.> "zip"
        signatureFile = zipFile <.> "signature"
        qualZipFile = tmpDir </> zipFile
        qualSignatureFile = tmpDir </> signatureFile
    downloadFile app logCommand (mkUrl repo zipFile) qualZipFile
    downloadFile app logCommand (mkUrl repo signatureFile) qualSignatureFile
    io $ logCommand (p "verifying signature")
    verifyUpdate qualZipFile qualSignatureFile
    return $ ZipFilePath (tmpDir </> zipFile)

-- | unzips a given zipFile (in the same directory) and returns the path to the unzipped directory
unzipFile :: Application -> (Prose -> IO ()) -> ZipFilePath
    -> ErrorT [String] IO NewVersionDir
unzipFile app logCommand (ZipFilePath path) = do
    io $ logCommand (p "uncompressing " `mappend` pVerbatim (takeBaseName path))
    io $ unzipArchive path (takeDirectory path)
    let nikkiDir = takeDirectory path </> mkDeployedFolder "nikki"
    nikkiExists <- io $ doesDirectoryExist nikkiDir
    when (not nikkiExists) $ throwError ["directory not found:", nikkiDir]
    return $ NewVersionDir nikkiDir

-- | Backups all files to a temporary directory.
-- Restores them in case anything goes wrong.
-- Catches every exception and every ErrorT error.
-- Leaves the backup where it is (in a folder called "temporaryBackupSOMETHING",
-- which will be deleted by the restarter at a later launch.)
withBackup :: Application -> (Prose -> IO ()) -> DeployPath
    -> ErrorT [String] IO a -> ErrorT [String] IO a
withBackup app logCommand (DeployPath deployPath) action = do
    deployedFiles <- io $ sort <$> getDirectoryRealContents deployPath
    tmpDir <- io $ createTempDirectory deployPath "temporaryBackup"

    let backup :: ErrorT [String] IO ()
        backup = do
            forM_ deployedFiles $ \ f ->
                rename (deployPath </> f) (tmpDir </> f)
        restore :: ErrorT [String] IO ()
        restore = do
            io $ logCommand (p "restoring backup")
            forM_ deployedFiles $ \ f -> do
                let dest = deployPath </> f
                io $ removeIfExists dest
                rename (tmpDir </> f) dest

    backup
    result <- catchError
                (action `onException` restore)
                (\ errorMessage -> restore >> throwError errorMessage)
    return result
  where

    -- | renaming directories and files
    rename :: FilePath -> FilePath -> ErrorT [String] IO ()
    rename src dest = do
        isFile <- io $ doesFileExist src
        isDirectory <- io $ doesDirectoryExist src
        if isFile then
            io $ renameFile src dest
          else if isDirectory then
            io $ renameDirectory src dest
          else
            throwError ["file not found: " ++ src]

-- | installs the update
installUpdate :: NewVersionDir -> DeployPath -> ErrorT [String] IO ()
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
