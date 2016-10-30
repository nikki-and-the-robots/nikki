{-# language DeriveFunctor, DeriveFoldable #-}

module Data.StrictList where

import           Control.Monad (ap)
import           Data.Foldable as F

data StrictList a
    = !a :! !(StrictList a)
    | Empty
  deriving (Show, Functor, Foldable)

infixr 5 :!

instance Monoid (StrictList a) where
    mempty = Empty
    mappend (a :! r) bs = a :! mappend r bs
    mappend Empty bs = bs

instance Applicative StrictList where
    (<*>) = ap
    pure = return

instance Monad StrictList where
    m >>= k = F.foldr (mappend . k) Empty m
    m >> k = F.foldr (mappend . (\ _ -> k)) Empty m
    return x = singleton x
    fail _ = Empty

type SL a = StrictList a

fromList :: [a] -> StrictList a
fromList (a : r) = a :! fromList r
fromList [] = Empty

singleton :: a -> StrictList a
singleton = (:! Empty)

(!) :: StrictList a -> Int -> a
l ! n = case (l, n) of
    ((a :! r), 0) -> a
    ((_ :! r), i) -> r ! pred i
    (Empty, _) -> error "(!): index out of bounds"

slFilter :: (a -> Bool) -> StrictList a -> StrictList a
slFilter p (a :! r) =
    (if p a then (a :!) else id) (slFilter p r)
slFilter _ Empty = Empty

filterM :: Monad m => (a -> m Bool) -> StrictList a -> m (StrictList a)
filterM pred (a :! r) = do
    c <- pred a
    r' <- filterM pred r
    if c then do
        r' <- filterM pred r
        return (a :! r')
      else do
        r' <- filterM pred r
        return r'
filterM pred Empty = return Empty


-- | PRE: SL a is sorted.
-- Inserts an element to a list. The returned list will be sorted.
-- If the input list contained only unique elements, the output list will
-- do also. (This means, the element will not be inserted,
-- if it's already in the list.)
insertUnique :: Ord a => a -> SL a -> SL a
insertUnique a (b :! r) =
    case compare a b of
        GT -> b :! insertUnique a r
        EQ -> b :! r -- not inserted
        LT -> a :! b :! r
insertUnique a Empty = singleton a

delete :: Eq a => a -> SL a -> SL a
delete x (a :! r) =
    if x == a
    then r
    else a :! delete x r
delete x Empty = Empty
