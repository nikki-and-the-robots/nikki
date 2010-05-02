{-# language MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Compose (
    (>=>),
    (<=<),

    pure,
    passThrough,
  ) where


(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
x >=> y = \ a -> x a >>= y

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)

-- lifter stuff

pure :: Monad m => (a -> b) -> (a -> m b)
pure = (return .)

passThrough :: Monad m => (a -> m ()) -> (a -> m a)
passThrough cmd a = cmd a >> return a

