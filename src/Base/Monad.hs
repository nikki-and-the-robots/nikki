
-- | defines the main monad type of the application

module Base.Monad (
    module Base.Monad,

    asks,
    ask,
  ) where


import Control.Monad.Trans.Reader

import Base.Configuration


type M = ReaderT Configuration IO

-- | reads the configuration
-- configuration should not be changed
withStaticConfiguration :: M a -> IO a
withStaticConfiguration action =
    runReaderT action =<< getConfiguration

-- | Executes an M Monad.
-- Will save changes to the configuration afterwards
-- (Once this is possible, for now M is just ReaderT Configuration IO)
withDynamicConfiguration :: Configuration -> M a -> IO a
withDynamicConfiguration configuration action =
    runReaderT action configuration
