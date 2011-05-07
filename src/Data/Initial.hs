
module Data.Initial where


import Data.Map
import Data.IntMap


class Initial d where
    initial :: d

instance Initial () where
    initial = ()

instance Initial [a] where
    initial = []

instance Initial (Data.Map.Map k a) where
    initial = Data.Map.empty

instance Initial (Data.IntMap.IntMap a) where
    initial = Data.IntMap.empty
