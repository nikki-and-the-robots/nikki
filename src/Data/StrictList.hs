{-# language DeriveFunctor, DeriveFoldable #-}

module Data.StrictList where


import Data.Monoid
import Data.Foldable as F


data StrictList a
    = !a :$ !(StrictList a)
    | Empty
  deriving (Show, Functor, Foldable)

infixr 5 :$

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


insert :: Ord a => a -> SL a -> SL a
insert a (b :$ r) =
    if a > b
    then b :$ insert a r
    else a :$ b :$ r
insert a Empty = singleton a

delete :: Eq a => a -> SL a -> SL a
delete x (a :$ r) =
    if x == a
    then r
    else a :$ delete x r
delete x Empty = Empty
