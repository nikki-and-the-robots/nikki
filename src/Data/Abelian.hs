{-# language NamedFieldPuns #-}

module Data.Abelian where

-- * Algebra (classes and instances)

class Abelian a where
    zero :: a
    (+~) :: a -> a -> a
    (-~) :: a -> a -> a
    negateAbelian :: a -> a
    negateAbelian = (zero -~)

instance Abelian Int where
    zero = 0
    (+~) = (+)
    (-~) = (-)

instance Abelian Double where
    zero = 0
    (+~) = (+)
    (-~) = (-)

