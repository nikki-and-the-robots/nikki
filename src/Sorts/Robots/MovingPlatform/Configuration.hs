{-# language ScopedTypeVariables #-}

module Sorts.Robots.MovingPlatform.Configuration where


import Base

import Sorts.Nikki as Nikki (nikkiMass)


-- | The mass of platforms.
-- (gravity has no effect on platforms
platformMass :: Double = nikkiMass * 3.5 -- 3

platformFriction :: Double = 0.75 -- 0.95

-- | general velocity of platforms
platformStandardVelocity :: Double = 170 -- 150


-- * spring configuration

data SpringConfiguration = SpringConfiguration {
    -- When the platform is further than this value away from its aim,
    -- the applied acceleration will have reached platformAcceleration
    springAcceleration :: Double,
    -- factor of friction
    -- (not dependent on the velocity, like sliding friction)
    frictionFactor :: Double,
    -- factor of drag
    -- (dependent on velocity, like air drag)
    dragFactor :: Double
  }

-- | spring configuration for platforms that move on a path
pathSpringConfiguration = SpringConfiguration {
    springAcceleration = 2550, -- 4249.984,
    frictionFactor = 85,
    dragFactor = 1190
  }

-- | When the platforms are switched off or if there is just one path node.
singleNodeSpringConfiguration = SpringConfiguration {
    springAcceleration = 3500, -- 4249.984,
    frictionFactor = 85,
    dragFactor = 170
  }
