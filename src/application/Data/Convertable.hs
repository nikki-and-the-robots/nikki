{-# language MultiParamTypeClasses #-}

module Data.Convertable where


import Utils

import Data.Map (Map, fromList, toList)


class Convertable a b where
    convert :: a -> b

instance (Convertable a b, Convertable x y, Ord y) => 
    Convertable (Map x a) (Map y b) where
        convert = Data.Map.toList >>> convert >>> Data.Map.fromList

instance (Convertable a b, Convertable x y) => Convertable (x, a) (y, b) where
    convert (a, b) = (convert a, convert b)

instance Convertable a b => Convertable [a] [b] where
    convert = map convert

