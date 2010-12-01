{-# language ScopedTypeVariables #-}

-- | contains configuration values for all robots

module Sorts.Robots.Configuration where


import Graphics.Qt

import Base.Constants


-- * physics

-- | robot mass per (square-)pixel
robotMaterialMass :: Double = 78

robotBodySize :: Size Double = fmap fromUber $ Size 15 15


-- * animation speeds

robotIdleEyeTime :: Double 
robotIdleEyeTime = 0.4
