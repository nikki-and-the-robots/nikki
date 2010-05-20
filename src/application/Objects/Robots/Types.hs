
module Objects.Robots.Types where


import Utils

import Data.Directions

import Physics.Chipmunk

import Game.Animation


data RobotState
    = JetpackState {
        robotBoost :: Bool,
        robotDirection :: Maybe HorizontalDirection,
        robotAnimation :: Animation
      }
    | LaserRobot {
        robotLasers :: [Laser],
        robotAnimation :: Animation,
        robotLaserAnimation :: Animation
      }
    | LaserEndRobot {
        robotLaserDirection :: Direction
      }
    | ProtoRobot
  deriving (Show)

robotWaitAnimation :: AnimationPhases
robotWaitAnimation = AnimationPhases $ zip
    (cycle [0, 1])
    (cycle [3, 0.15, 0.1, 0.15])
  where
    blink _ 0 = e "blink"
    blink waitSeconds numberOfBlinks =
        waitSeconds : blinkTime : repeatList (numberOfBlinks - 1) [blinkTime, blinkTime]

    blinkTime = 0.1

robotIdleEyeTime :: Double
robotIdleEyeTime = 0.4



-- * laserRobot

data Laser = Laser {
    laserDirection :: Direction,
    laserShapes :: Maybe [Shape],
    switchedOn :: Bool,
    laserLength :: Int -- in uberpixels
  }
    deriving Show

initialLaserRobot :: [Direction] -> RobotState
initialLaserRobot directions =
    LaserRobot
        (zipWith initialLaser directions (initialLaserSwitches directions))
        UninitializedAnimation
        UninitializedAnimation

-- | which lasers are on initially
initialLaserSwitches :: [Direction] -> [Bool]
initialLaserSwitches [x] = [True]
initialLaserSwitches [DUp, DRight] = [True, False]
initialLaserSwitches x = es "initialLaserSwitches" x

initialLaser :: Direction -> Bool -> Laser
initialLaser direction on = Laser direction Nothing on 0




