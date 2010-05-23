{-# language ViewPatterns, NamedFieldPuns #-}

module Objects.Robots (
    convertObject,
    Objects.Robots.initChipmunk,
    Objects.Robots.initAnimation,
    update,
    render,
  ) where


import Prelude hiding (lookup)

import Data.Directions

import Graphics.Qt as Qt

import Physics.Chipmunk as CM


import Utils

import Base.Events
import Base.Sprited
import Base.PickleObject

import Game.Scene.Types

import Objects.Robots.Types as RobotTypes
import Objects.Robots.Handler as Handler
import Objects.Collisions
import Objects.Animation
import Objects.Types

import qualified Objects.Robots.Jetpack as Jetpack
import qualified Objects.Robots.Laser as Laser
import qualified Objects.Robots.TestRobot as TestRobot



-- * glue code for Game.Objects.General

convertObject :: (Show s, SpritedClass s) => EObject_ s -> Object_ s Vector
convertObject (ERobot pos sprited) =
    let typ = case spritedName sprited of
            (getName -> "robot-jetpack") -> Jetpack.initialState
            (getName -> "robot-prototype") -> ProtoRobot

            (getName -> "robot-laser-up") -> initialLaserRobot [DUp]
            (getName -> "robot-laser-up-right") -> initialLaserRobot [DUp, DRight]

            (getName -> "robot-laser-end-up") -> LaserEndRobot DUp
            (getName -> "robot-laser-end-right") -> LaserEndRobot DRight

            name -> es "convertObject" name
    in Robot sprited (positionToVector pos) typ

initChipmunk :: UninitializedScene -> Space -> UninitializedObject -> IO Object
initChipmunk scene s r = handle initialisation r scene s r

initAnimation :: Object -> Object
initAnimation r = handle Handler.initAnimation r r

render :: Ptr QPainter -> Scene -> Qt.Position Double -> Object -> IO ()
render p s o r = handle rendering r p s o r

update :: Scene -> Seconds -> Collisions -> (Bool, ControlData)
    -> Object -> IO Object
update scene now collisions cd r = handle updating r scene now collisions cd r

handle :: (RobotHandler -> a) -> Object_ x y -> a
handle getter (robotHandlers . robotState -> handler) = getter handler


-- * actual handlers

robotHandlers :: RobotState -> RobotHandler
robotHandlers JetpackState{} = Jetpack.jetpackRobotHandler
robotHandlers LaserRobot{} = Laser.laserRobotHandler
robotHandlers LaserEndRobot{} = Laser.laserEndRobotHandler

robotHandlers ProtoRobot{} = TestRobot.handler

robotHandlers x = es "Sorry, no handler for this robot type found" x


