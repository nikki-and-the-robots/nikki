{-# language ScopedTypeVariables #-}

module Sorts.Robots.MovingPlatform.Configuration where


import Base

import Sorts.Nikki as Nikki (nikkiMass)


-- | The mass of platforms.
-- (gravity has no effect on platforms
platformMass :: Double = nikkiMass * 4

-- | the acceleration that can will applied to a platform
-- to let it follow its path
platformAcceleration :: Double = 1300

-- | sets the epsilon range for the velocity correction
velocityEpsilon :: Double = 5

-- | general velocity of platforms
platformStandardVelocity :: Double = 150

-- | Factor with which the impulse at the path nodes will be applied.
-- 1.0 = perfect (hard) direction changes
-- 0.0 = no edge impulse applied at all.
edgeImpulseFactor :: Double = 0.0

-- * chasing mode

-- | If the platform is more than positionEpsilon from its aim away,
-- it will accelerate to platformMaximumVelocity,
-- thus going into chasing mode.
positionEpsilon :: Double = 5

-- | The maximal velocity a platform can have when chasing the so called
-- guide point (to get in sync again)
platformMaximumVelocity :: Double = 200 -- 180

-- | Distance the aim will be away from the closest point on the path during chasing.
-- (Smaller = go back to the path faster,
--  Greater = go more directly to the end of the current segment)
-- Only relevant during chasing.
aimDistance :: Double = 50

