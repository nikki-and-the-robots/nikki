{-# language FlexibleInstances, NamedFieldPuns #-}

module Objects.Types (
    robotFriction,

    UninitializedObject,
    UnloadedObject,
    Object,
    Object_(..),
    MergedSprited(..),
    TerminalLight(..),

    allTerminalLights,
    initialTerminalLights,

    isTerminal,
    isNikki,
    isRobot,
    isLaserEndRobot,

    getObjectPosition,

    setAnimation,

    modifyTerminalSelected,
    modifyTerminalState,
    modifyTerminalStateM,
    modifyRobotStateM,
    modifyRobotState,
    modifyNikkiState,

    loadSpriteds,
    objectLoadSpriteds,
    initDummyChipmunk
  ) where

import Utils

import Control.Monad.FunctorM
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative ((<$>))
import Control.Exception

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Sound.SFML

import Objects.Animation
import Objects.Robots.Types
import qualified Objects.Nikki.Types as Nikki
import Objects.Terminals.Types as Terminals

import Base.Sprited


-- * Constants

robotFriction :: Double
robotFriction = 1.0


-- * Types

type UnloadedObject = Object_ UnloadedSprited Vector

type UninitializedObject = Object_ Sprited Vector

type Object = Object_ Sprited Chipmunk

data Object_ sprited chipmunk =
      Tile {
        sprited :: sprited,
        chipmunk :: chipmunk,
        animation :: Animation
      }
    | MergedTile {
        merged :: [MergedSprited],
        chipmunk :: chipmunk
    }
    | Nikki {
        sprited :: sprited,
        chipmunk :: chipmunk,
        feetShape :: Maybe Shape,
        jumpSound :: Maybe PolySound,
        jumpTime :: Seconds,
        nikkiState :: Nikki.State
      }
    | Robot {
        sprited :: sprited,
        chipmunk :: chipmunk,
        robotState :: RobotState
      }
    | Terminal {
        sprited :: sprited,
        chipmunk :: chipmunk,
        terminalState :: Terminals.State
      }
    | MilkMachine {
        sprited :: sprited,
        chipmunk :: chipmunk,
        animation :: Animation
      }
    | Box {
        sprited :: sprited,
        chipmunk :: chipmunk
      }

    | OSDObject {
        sprited :: sprited
      }
  deriving Show

data MergedSprited = MergedSprited {
    mergedSprited :: Sprited,
    mergedOffset :: (Qt.Position Double),
    mergedAnimation :: Animation
  }
    deriving Show


-- * constructors

initialTerminalLights :: Object_ a c -> Object_ a c
initialTerminalLights t@Terminal{terminalState} =
    t{terminalState = terminalState{terminalLights}}
  where
    terminalLights = take (terminalLength terminalState) allTerminalLights


-- * Discriminators

isNikki :: Object_ a b -> Bool
-- isNikki x = es "isNikki" x
isNikki Nikki{} = True
isNikki _ = False

isRobot :: Object_ a b -> Bool
isRobot Robot{} = True
isRobot _ = False

isTerminal :: Object -> Bool
isTerminal Terminal{} = True
isTerminal _ = False

isLaserEndRobot :: Object_ a b -> Bool
isLaserEndRobot Robot{robotState = LaserEndRobot{}} = True
isLaserEndRobot _ = False


-- * getters

getObjectPosition :: Object -> IO (Maybe (CM.Position, Angle))
getObjectPosition Nikki{chipmunk} = Just <$> getRenderPosition chipmunk
getObjectPosition Tile{chipmunk} = Just <$> getRenderPosition chipmunk
getObjectPosition Robot{chipmunk} = Just <$> getRenderPosition chipmunk
getObjectPosition x = es "getObjectPosition" x


-- * setters

setSprited :: Object_ a c -> b -> Object_ b c
setSprited o x = o{sprited = x}

setAnimation :: Object -> Animation -> Object
setAnimation o@Tile{} a = assert (isUninitializedAnimation $ animation o) result
  where
    result = o{animation = a}

-- * modifications

-- | sets the terminal while clipping to the range of robots.
-- should only be used, when actually CHANGING the value
-- PRE: forall x . f x /= x
modifyTerminalSelected :: (Int -> Int) -> Object -> Object
modifyTerminalSelected f t@Terminal{} =
    modifyTerminalState (Terminals.modifySelected f) t

modifyTerminalState :: (Terminals.State -> Terminals.State)
    -> Object_ a c -> Object_ a c
modifyTerminalState f t@Terminal{terminalState} =
    t{terminalState = f terminalState}

modifyTerminalStateM :: Monad m => (Terminals.State -> m Terminals.State)
    -> Object_ a c -> m (Object_ a c)
modifyTerminalStateM f t@Terminal{terminalState} = do
    state' <- f terminalState
    return $ t{terminalState = state'}

modifyRobotStateM :: Monad m => (RobotState -> m RobotState) -> Object -> m Object
modifyRobotStateM f r@Robot{robotState} =
    f robotState ~> (\ robotState' -> r{robotState = robotState'})

modifyRobotState :: (RobotState -> RobotState) -> Object -> Object
modifyRobotState f r = r{robotState = f (robotState r)}

modifyNikkiState :: (Nikki.State -> Nikki.State) -> Object -> Object
modifyNikkiState f nikki@Nikki{nikkiState} =
    nikki{nikkiState = f nikkiState}


-- * load pixmaps

-- | convenience Function for FunctorM
loadSpriteds :: FunctorM f => f UnloadedObject -> IO (f UninitializedObject)
loadSpriteds x = evalStateT (fmapM objectLoadSpriteds x) Map.empty

objectLoadSpriteds :: UnloadedObject -> StateT PixMap IO UninitializedObject
objectLoadSpriteds o = do
    sprited' <- loadSprited (sprited o)
    return $ setSprited o sprited'
-- objectLoadSpriteds x = es "objectLoadSpriteds" x

-- * dummy chipmunks

-- | initialises an Object with a chipmunk Value, that will not be registered with the
-- chipmunk lib.
initDummyChipmunk :: UninitializedObject -> Object
initDummyChipmunk (OSDObject s) = OSDObject s
initDummyChipmunk o =
    o{chipmunk = DummyChipmunk position}
  where
    position = chipmunk o



