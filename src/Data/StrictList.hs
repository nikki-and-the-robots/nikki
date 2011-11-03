{-# language DeriveFunctor, DeriveFoldable #-}

module Data.StrictList where


import Data.Foldable


data StrictList a
    = !a :$ !(StrictList a)
    | Empty
  deriving (Show, Functor, Foldable)

type SL a = StrictList a

fromList :: [a] -> StrictList a
fromList (a : r) = a :$ fromList r
fromList [] = Empty

(!) :: StrictList a -> Int -> a
(a :$ r) ! 0 = a
(_ :$ r) ! i = r ! pred i
Empty ! _ = error "(!): index out of bounds"
