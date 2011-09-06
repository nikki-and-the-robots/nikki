
module Data.Convertable where


import Utils

import Data.Map (Map, fromList, toList)
import qualified Data.Strict


class Convertable a b where
    convert :: a -> b

instance (Convertable a b, Convertable x y, Ord y) => 
    Convertable (Map x a) (Map y b) where
        convert = Data.Map.toList >>> convert >>> Data.Map.fromList

instance (Convertable a b, Convertable x y) => Convertable (x, a) (y, b) where
    convert (a, b) = (convert a, convert b)

instance Convertable a b => Convertable [a] [b] where
    convert = map convert

-- * Data.Strict

instance Convertable (Maybe a) (Data.Strict.Maybe a) where
    convert (Just a) = Data.Strict.Just a
    convert Nothing = Data.Strict.Nothing

instance Convertable (Data.Strict.Maybe a) (Maybe a) where
    convert (Data.Strict.Just a) = Just a
    convert Data.Strict.Nothing = Nothing
