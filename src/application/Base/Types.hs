{-# language NamedFieldPuns, FlexibleInstances #-}


-- module for often used types (in one Base module, to avoid module import cycles.)

module Base.Types where


import Data.Set
import Data.Indexable
import Data.Initial
import Data.Abelian
import Data.Array.Storable

import Physics.Chipmunk

import Utils

import Base.Constants
import Base.Grounds


data Scene object
    = Scene {
        now :: Seconds,
        oldNow :: Seconds,
        objects :: Grounds object,
        cameraState :: CameraState,
        contactRef :: !(ContactRef Contacts),
        contacts :: !Contacts,
        mode :: Mode
      }
  deriving Show
  
-- * getter

-- | returns the object currently controlled by the gamepad
getControlled :: Scene o -> o
getControlled s = s |> getControlledIndex |> getMainlayerObject s

getControlledIndex :: Scene o -> Index
getControlledIndex Scene{mode} =
    case mode of
        NikkiMode{nikki} -> nikki
        TerminalMode{terminal} -> terminal
        RobotMode{robot} -> robot

-- | returns an object from the mainLayer
getMainlayerObject :: Scene o -> Index -> o
getMainlayerObject s@Scene{objects} i = mainLayerIndexable objects !!! i


-- * modifications

modifyMainlayerObjectByIndex :: (o -> o) -> Index -> Scene o -> Scene o
modifyMainlayerObjectByIndex f i =
    modifyObjects (modifyMainLayer (modifyByIndex f i))

modifyObjects :: (Grounds a -> Grounds b) -> Scene a -> Scene b
modifyObjects f s@Scene{objects} =
    s{objects = f objects}

modifyMode :: (Mode -> Mode) -> Scene o -> Scene o
modifyMode f s@Scene{mode} = s{mode = f mode}


data CameraState
    = CS Vector
  deriving Show

instance Initial CameraState where
    initial = CS zero



data Contacts
    = Contacts {
        nikkiContacts :: [(StorableArray Int Contact, Double)],
        nikkiTouchesLaser :: !Bool,
        nikkiTouchesMilkMachine :: !Bool,
        terminals :: Set Shape,
        batteries :: Set Shape,
        fallingTiles :: Set Shape
      }
  deriving Show

instance Show (StorableArray Int Contact) where
    show = const "<StorableArray>"


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


data LevelResult = Passed | Failed
  deriving Show

