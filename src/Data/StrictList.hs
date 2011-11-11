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

slFilter :: (a -> Bool) -> StrictList a -> StrictList a
slFilter p (a :$ r) =
    (if p a then (a :$) else id) (slFilter p r)
slFilter _ Empty = Empty

filterM :: Monad m => (a -> m Bool) -> StrictList a -> m (StrictList a)
filterM pred (a :$ r) = do
    c <- pred a
    r' <- filterM pred r
    if c then do
        r' <- filterM pred r
        return (a :$ r')
      else do
        r' <- filterM pred r
        return r'
filterM pred Empty = return Empty


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
