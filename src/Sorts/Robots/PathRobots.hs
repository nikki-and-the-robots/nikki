
-- | Path robots are:
--      platform robots
--      patrol robots

module Sorts.Robots.PathRobots where


import Base

import qualified Sorts.Robots.PathRobots.Platforms as Platforms



-- * loading

sorts :: RM [Sort_]
sorts = do
    platformSort <- Platforms.sort
    return (
        platformSort :
        [])
