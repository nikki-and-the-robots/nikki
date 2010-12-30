
-- | defines the main monad type of the application

module Base.Monad (
    M,
    asks,
    ask,
  ) where


import Control.Monad.Trans.Reader

import Utils

import Base.Configuration


type M = ReaderT Configuration IO
