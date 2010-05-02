
module Control.Monad.FunctorM where


import Utils

import Data.Map


class FunctorM f where
    fmapM :: Monad m => (a -> m b) -> f a -> m (f b)
    fmapM_ :: Monad m => (a -> m b) -> f a -> m ()

instance FunctorM [] where
    fmapM f (a : r) = do
        fa <- f a
        fr <- fmapM f r
        return (fa : fr)
    fmapM f [] = return []
    fmapM_ = e "fmapM_ in []"

instance FunctorM Maybe where
    fmapM f Nothing = return Nothing
    fmapM f (Just x) = f x >>= (return . Just)
    fmapM_ f Nothing = return ()
    fmapM_ f (Just x) = do
        _ <- f x
        return ()

instance Ord a => FunctorM (Map a) where
    fmapM f m =
        inner (toList m) >>= return . fromList
      where
        inner ((k, e) : r) = do
            e' <- f e
            r' <- inner r
            return ((k, e') : r')
        inner [] = return []
    fmapM_ = error "FunctorM.hs"




