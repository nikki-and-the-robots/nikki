{-# language ScopedTypeVariables #-}

-- | contains configuration values for all robots

module Sorts.Robots.Configuration where


import Graphics.Qt

import Base.Constants


-- * physics

-- | robot mass per (square-)pixel
robotMass :: Double = 64 / kachelArea
  where
    kachelArea = fromUber 16 ^ 2

robotBodySize :: Size Double = fmap fromUber $ Size 15 15


-- * animation speeds

robotIdleEyeTime :: Double 
robotIdleEyeTime = 0.4
