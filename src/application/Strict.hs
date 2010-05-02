{-# language MultiParamTypeClasses #-}

module Strict where


class Strict a b where
    toStrict :: a -> b



data Maybe a = Nothing | Just !a
  deriving Show

instance Strict (Prelude.Maybe a) (Strict.Maybe a) where
    toStrict = error "toStrict"

-- stolen from base package
instance  Monad Strict.Maybe  where
    (Strict.Just x) >>= k      = k x
    Strict.Nothing  >>= _      = Strict.Nothing

    (Strict.Just _) >>  k      = k
    Strict.Nothing  >>  _      = Strict.Nothing

    return              = Strict.Just
    fail _              = Strict.Nothing
