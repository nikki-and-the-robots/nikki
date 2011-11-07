
-- | Path robots are:
--      platform robots
--      patrol robots

module Sorts.Robots.PathRobots where


import Utils

import Base

import qualified Sorts.Robots.PathRobots.Platform as Platform
import qualified Sorts.Robots.PathRobots.PatrolRobot as PatrolRobot



-- * loading

sorts :: [RM (Maybe Sort_)]
sorts =
    (Just <$> Platform.sort) :
    (Just <$> PatrolRobot.sort) :
    []
