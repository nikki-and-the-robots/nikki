
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


-- * changing the dynamic configuration

swapFullScreen :: Application -> M ()
swapFullScreen app = do
    fullscreen' <- not <$> gets fullscreen
    modify (\ c -> c{fullscreen = fullscreen'})
    io $ postGUI (window app) $ do
        setFullscreenGLContext (window app) fullscreen'
