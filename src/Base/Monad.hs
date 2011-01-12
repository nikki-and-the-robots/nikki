
-- | defines the main monad type of the application

module Base.Monad (
    module Base.Monad,

    ask,
    asks,
    gets,
  ) where


import Control.Monad.Reader
import Control.Monad.State.Strict

import Utils

import Base.Configuration


type ConfigurationReader = ReaderT Configuration IO
type RM = ConfigurationReader

type ConfigurationState = StateT Configuration IO
type M = ConfigurationState


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
