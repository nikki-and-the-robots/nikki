
-- | Path robots are:
--      platform robots
--      patrol robots

module Sorts.Robots.PathRobots where

import           Base
import qualified Sorts.Robots.PathRobots.PatrolRobot as PatrolRobot
import qualified Sorts.Robots.PathRobots.Platform as Platform

-- * loading

sorts :: [IO (Maybe Sort_)]
sorts =
    (Just <$> Platform.sort) :
    (Just <$> PatrolRobot.sort) :
    []
