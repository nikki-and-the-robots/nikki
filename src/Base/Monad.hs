
-- | defines the main monad type of the application

module Base.Monad (
    module Base.Monad,

    ask,
    asks,
    gets,
  ) where


import Control.Monad.Reader
import Control.Monad.State.Strict

import Graphics.Qt

import Utils

import Base.Configuration
import Base.Types


getConfiguration :: M Configuration
getConfiguration = get

rm2m :: RM a -> M a
rm2m action = (io . runReaderT action) =<< getConfiguration

-- | reads the configuration
-- configuration should not be changed
withStaticConfiguration :: ConfigurationReader a -> IO a
withStaticConfiguration action =
    runReaderT action =<< loadConfiguration

-- | Executes an M Monad.
-- Will save changes to the configuration afterwards
-- (Once this is possible, for now M is just ReaderT Configuration IO)
withDynamicConfiguration :: Configuration -> M a -> IO a
withDynamicConfiguration configuration action = do
    (o, newConfig) <- runStateT action configuration
    -- TODO: save config
    return o


-- * changing the dynamic configuration

swapFullScreen :: Application_ s -> M ()
swapFullScreen app = do
    fullscreen' <- not <$> gets fullscreen
    modify (\ c -> c{fullscreen = fullscreen'})
    io $ setFullscreenAppWidget (window app) fullscreen'
