{-# language ScopedTypeVariables #-}

-- | Contains configuration values for all robots.

module Sorts.Robots.Configuration where


import Physics.Chipmunk as CM

import Graphics.Qt

import Base.Types
import Base.Constants


-- * physics

-- | robot mass per (square-)pixel
robotMaterialMass :: Double = 78

-- | physical attributes of the robots
-- (Obviously, don't change the collisionType.)
robotShapeAttributes = ShapeAttributes{
    elasticity = 0.8,
    friction = 0.5,
    CM.collisionType = RobotCT
  }

-- | size of one of the robots "face tiles"
robotBodySize :: Size Double = fmap fromUber $ Size 15 15


-- * animation speeds

robotIdleEyeTime :: Double 
robotIdleEyeTime = 0.4
