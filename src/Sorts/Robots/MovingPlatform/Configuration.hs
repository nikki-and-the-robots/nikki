{-# language ScopedTypeVariables #-}

module Sorts.Robots.MovingPlatform.Configuration where


import Base

import Sorts.Nikki as Nikki (nikkiMass)


-- | The mass of platforms.
-- (gravity has no effect on platforms
platformMass :: Double = nikkiMass * 3

-- | the acceleration that can will applied to a platform
-- to let it follow its path
platformAcceleration :: Double = 1700

-- | general velocity of platforms
platformStandardVelocity :: Double = 150


-- * spring configuration

data SpringConfiguration = SpringConfiguration {
    -- When the platform is further than this value away from its aim,
    -- the applied acceleration will have reached platformAcceleration
    springConstantAccelerationDistance :: Double,
    -- factor of friction
    -- (not dependent on the velocity, like sliding friction)
    frictionFactor :: Double,
    -- factor of drag
    -- (dependent on velocity, like air drag)
    dragFactor :: Double
  }

-- | spring configuration for platforms that move on a path
pathSpringConfiguration = SpringConfiguration {
    springConstantAccelerationDistance = fromKachel 0.4,
    frictionFactor = 0.05,
    dragFactor = 0.7
  }

-- | When the platforms are switched off or if there is just one path node.
singleNodeSpringConfiguration = SpringConfiguration {
    springConstantAccelerationDistance = fromKachel 0.4,
    frictionFactor = 0.05,
    dragFactor = 0.1
  }
