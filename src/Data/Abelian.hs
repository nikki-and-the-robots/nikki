
module Data.Abelian where

-- * Algebra (classes and instances)

class Abelian a where
    zero :: a
    (+~) :: a -> a -> a
    (-~) :: a -> a -> a
    negateAbelian :: a -> a
    negateAbelian = (zero -~)

infixl 6  +~, -~

instance Abelian Int where
    zero = 0
    (+~) = (+)
    (-~) = (-)

instance Abelian Double where
    zero = 0
    (+~) = (+)
    (-~) = (-)

instance Abelian Float where
    zero = 0
    (+~) = (+)
    (-~) = (-)


class VectorLike t where
    componentWise :: (a -> b -> c) -> t a -> t b -> t c
    split :: a -> t a
