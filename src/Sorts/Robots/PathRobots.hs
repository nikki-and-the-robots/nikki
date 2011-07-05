
-- | Path robots are:
--      platform robots
--      patrol robots

module Sorts.Robots.PathRobots where


import Base

import qualified Sorts.Robots.PathRobots.Platform as Platform



-- * loading

sorts :: RM [Sort_]
sorts = do
    platformSort <- Platform.sort
    return (
        platformSort :
        [])
