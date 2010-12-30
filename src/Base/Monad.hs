
-- | defines the main monad type of the application

module Base.Monad (
    M,
    asks,
    ask,
  ) where


import Control.Monad.Reader

import Base.Configuration


type M = ReaderT Configuration IO
