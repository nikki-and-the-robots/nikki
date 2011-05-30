{-# language FlexibleInstances, MultiParamTypeClasses #-}

-- | state monad that can be caught

module Control.Monad.CatchState (
    CatchState,
    runCatchState,
  ) where


import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad.CatchIO as CatchIO

import Control.Concurrent.MVar

import Utils


-- | Uses an MVar internally to access the state.
-- Makes sure, the MVar is not empty for a longer time
-- (nor when an exception can be thrown.)
data CatchState state m a =
    CatchState {innerAction :: (MVar state -> m a)}

instance Monad m => Monad (CatchState state m) where
    (CatchState a) >>= b =
        CatchState $ \ mvar -> do
            tmp <- a mvar
            innerAction (b tmp) mvar
    return x = CatchState (const $ return x)
instance MonadCatchIO m => MonadState state (CatchState state m) where
    get = CatchState $ \ mvar ->
        block $ io $ readMVar mvar
    put state = CatchState $ \ mvar ->
        block $ io $ ignore $ swapMVar mvar state
instance MonadIO m => MonadIO (CatchState state m) where
    liftIO = CatchState . const . liftIO
instance Functor m => Functor (CatchState state m) where
    fmap f (CatchState action) = CatchState $ \ mvar ->
        fmap f (action mvar)
instance MonadCatchIO m => MonadCatchIO (CatchState state m) where
    catch (CatchState action) handler = CatchState $ \ mvar ->
        CatchIO.catch (action mvar) (\ e -> innerAction (handler e) mvar)
    block (CatchState action) = CatchState $ \ mvar ->
        CatchIO.block $ action mvar
    unblock (CatchState action) = CatchState $ \ mvar ->
        CatchIO.unblock $ action mvar

runCatchState :: MonadIO m => CatchState state m a -> state -> m (a, state)
runCatchState (CatchState action) state = do
    mvar <- io $ newMVar state
    a <- action mvar
    endState <- io $ takeMVar mvar
    return (a, endState)
