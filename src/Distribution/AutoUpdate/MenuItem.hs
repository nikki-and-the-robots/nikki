
module Distribution.AutoUpdate.MenuItem (
    autoUpdateMenuItem,
    mkUpdateVersionRef,
  ) where


import Data.Version

import Text.Logging

import Control.Monad
import Control.Monad.Trans.Error
import Control.Concurrent.MVar
import Control.Concurrent

import Graphics.Qt

import Utils

import Base

import Distribution.AutoUpdate


-- | Creates the MVar containing the UpdateVersion,
-- but also forks the thread that queries the UpdateVersion from
-- the update server.
mkUpdateVersionRef :: Ptr MainWindow -> Configuration -> IO (MVar (Maybe Version))
mkUpdateVersionRef window config = do
    mvar <- newEmptyMVar
    ignore $ forkIO $ do
        lookupUpdateVersion config >>= putMVar mvar
        updateMainWindow window
    return mvar

-- | Tries to lookup, if a newer version of the game is available.
-- Runs in the background of the main menu. In case of errors, this
-- operation does not terminate.
lookupUpdateVersion :: Configuration -> IO (Maybe Version)
lookupUpdateVersion config = do
    let repo = Repo $ update_repo config
    v <- io $ runErrorT $ getUpdateVersion repo
    case v of
        Left errors -> do
            mapM_ (logg Warning) errors
            io $ forever (threadDelay (1 * 10 ^ 6))
        Right x -> return x


autoUpdateMenuItem :: AutoUpdateMenuItem
autoUpdateMenuItem = AutoUpdateMenuItem id

data AutoUpdateMenuItem = AutoUpdateMenuItem (Prose -> Prose)

instance Renderable AutoUpdateMenuItem where
    render ptr app config size (AutoUpdateMenuItem proseMod) = do
        v <- tryReadMVar $ autoUpdateVersion app
        let r = render ptr app config size . proseMod
        case v of
            -- no information yet
            Nothing -> r $ p "online update"
            -- no update available
            Just Nothing -> r $ p "no update available"
            -- update available
            Just (Just newVersion) -> r $ substitute [("version", showVersion newVersion)] $
                p "new version available: $version"
    label = const "AutoUpdateMenuItem"
    select (AutoUpdateMenuItem f) = AutoUpdateMenuItem (select . f)
    deselect (AutoUpdateMenuItem f) = AutoUpdateMenuItem (deselect . f)

tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar mvar = do
    r <- tryTakeMVar mvar
    whenMaybe r $ putMVar mvar
    return r
