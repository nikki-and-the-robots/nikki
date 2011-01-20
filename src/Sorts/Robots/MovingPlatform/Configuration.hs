{-# language ScopedTypeVariables #-}

module Sorts.Robots.MovingPlatform.Configuration where


import Sorts.Nikki as Nikki (nikkiMass)


-- | The mass of platforms.
-- (gravity has no effect on platforms
platformMass :: Double = nikkiMass * 4
-- | the acceleration that can will applied to a platform
-- to let it follow its path
platformAcceleration :: Double = 1300
-- | the maximal velocity a platform can accelerate to
maximumPlatformVelocity :: Double = 180
-- | the minimal velocity a platform will decelerate to
-- at path nodes
minimumPlatformVelocity :: Double = 10
-- | sets the epsilon range for the velocity correction
velocityEpsilon :: Double = 5
-- | how much the platform tries to get back on its path
-- (as opposed to going to the end point of the active segment directly)
pathWeight :: Double = 0.66
