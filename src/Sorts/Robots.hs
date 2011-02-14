{-# language ViewPatterns #-}

module Sorts.Robots where


import Data.List

import Base


isRobot :: Sort sort o => sort -> Bool
isRobot (sortId -> (SortId s)) = "robots/" `isPrefixOf` s
