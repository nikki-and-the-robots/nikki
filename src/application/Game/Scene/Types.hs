{-# language NamedFieldPuns #-}

module Game.Scene.Types where


import Utils

import Data.Indexable

import Physics.Chipmunk

import Base.Grounds
import Base.Constants

import Object

import Game.Scene.Camera



data Scene
    = Scene {
        now :: Seconds,
        oldNow :: Seconds,
        objects :: Grounds Object_,
        cameraState :: CameraState,
        contacts :: !(ContactRef Contacts, Contacts),
        mode :: Mode
      }
  deriving Show

-- * mode for the game scene
data Mode
    = NikkiMode {
        nikki :: Index
      }
    | TerminalMode {
        nikki :: Index,
        terminal :: Index
      }
    | RobotMode{
        nikki :: Index,
        terminal :: Index,
        robot :: Index
      }
    | LevelFinished LevelResult
  deriving Show


data LevelResult = Passed | Failed
  deriving Show


-- * getter

getPassedTime :: Scene -> Seconds
getPassedTime x = es "getPassedTime" x

-- | returns the object currently controlled by the gamepad
getControlled :: Scene -> Object_
getControlled s = s |> getControlledIndex |> getMainObject s

getControlledIndex :: Scene -> Index
getControlledIndex Scene{mode} =
    case mode of
        NikkiMode{nikki} -> nikki
        TerminalMode{terminal} -> terminal
        RobotMode{robot} -> robot

-- | returns an object from the mainLayer
getMainObject :: Scene -> Index -> Object_
getMainObject s@Scene{objects} i = mainLayerIndexable objects !!! i

-- | returns, if Nikki is controlled currently
isNikkiMode :: Mode -> Bool
isNikkiMode NikkiMode{} = True

-- | returns, if a robot is controlled currently
isRobotMode :: Mode -> Bool
isRobotMode RobotMode{} = True
isRobotMode _ = False

isTerminalMode :: Mode -> Bool
isTerminalMode TerminalMode{} = True
isTerminalMode _ = False

-- * modifications

modifyMainByIndex :: (Object_ -> Object_) -> Index -> Scene -> Scene
modifyMainByIndex f i =
    modifyObjects (modifyMainLayer (modifyByIndex f i))

modifyObjects :: (Grounds Object_ -> Grounds Object_) -> Scene -> Scene
modifyObjects f s@Scene{objects} =
    s{objects = f objects}
-- 
-- modifyObjectsM :: Monad m => (Grounds Object_ -> m (Grounds Object_)) -> Scene -> m Scene
-- modifyObjectsM f scene@Scene{objects} = do
--     objects' <- f objects
--     return scene{objects = objects'}

modifyMode :: (Mode -> Mode) -> Scene -> Scene
modifyMode f s@Scene{mode} = s{mode = f mode}


-- modifyControlled :: (Object_ -> Object_)
--     -> Scene -> Scene
-- modifyControlled f s = setControlled s (f (getControlled s))

-- modifyControlledM :: Monad m => (Object_ -> m Object_) -> Scene -> m Scene
-- modifyControlledM f scene = do
--     c' <- f $ getControlled scene
--     return $ setControlled scene c'






