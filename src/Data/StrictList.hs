{-# language DeriveFunctor, DeriveFoldable #-}

module Data.StrictList where


import Data.Monoid
import Data.Foldable as F


data StrictList a
    = !a :$ !(StrictList a)
    | Empty
  deriving (Show, Functor, Foldable)

instance Monoid (StrictList a) where
    mempty = Empty
    mappend (a :$ r) bs = a :$ mappend r bs
    mappend Empty bs = bs

instance Monad StrictList where
    m >>= k = F.foldr (mappend . k) Empty m
    m >> k = F.foldr (mappend . (\ _ -> k)) Empty m
    return x = singleton x
    fail _ = Empty

type SL a = StrictList a

fromList :: [a] -> StrictList a
fromList (a : r) = a :$ fromList r
fromList [] = Empty

singleton :: a -> StrictList a
singleton = (:$ Empty)

(!) :: StrictList a -> Int -> a
(a :$ r) ! 0 = a
(_ :$ r) ! i = r ! pred i
Empty ! _ = error "(!): index out of bounds"
