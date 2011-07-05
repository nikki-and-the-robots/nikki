
-- | Path robots are:
--      platform robots
--      patrol robots

module Sorts.Robots.PathRobots where


import Base

import qualified Sorts.Robots.PathRobots.Platform as Platform
import qualified Sorts.Robots.PathRobots.PatrolRobot as PatrolRobot



-- * loading

sorts :: RM [Sort_]
sorts = do
    platformSort <- Platform.sort
    patrolSort <- PatrolRobot.sort
    return (
        platformSort :
        patrolSort :
        [])
