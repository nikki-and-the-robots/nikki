
-- | does general stuff in the game
-- this includes:
--  - chipmunk initialisation
--  - updating
--  - chipmunk stepping
--  - rendering

module Objects where


import Utils

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Base.Events
import Base.Sprited
import Base.PickleObject

import Game.Scene.Types

import Objects.Collisions
import Objects.Animation
import Objects.Types

import qualified Objects.Nikki as Nikki
import qualified Objects.Robots as Robots
import qualified Objects.Tiles as Tiles
import qualified Objects.Terminals as Terminals
import qualified Objects.MilkMachine as MilkMachine
import qualified Objects.Box as Box



-- * conversions from EditorObject

eObject2Object :: (Show s, SpritedClass s) => EObject_ s -> Object_ s Vector
eObject2Object (ETile p s) = Tile s (positionToVector p) UninitializedAnimation
eObject2Object o@(ENikki p s) = Nikki.convertObject o
eObject2Object (ETerminal p s i) = Terminals.initialTerminal p s i
eObject2Object r@ERobot{} = Robots.convertObject r
eObject2Object o@EMilkMachine{} = MilkMachine.convertObject o
eObject2Object (EBox p s) = Box s (positionToVector p)

-- eObject2Object x = es "eObject2Object" x


-- chipmunk init

-- | initialises the physical objects in chipmunk
-- (not the animations)
objectInitChipmunk :: UninitializedScene -> Space -> UninitializedObject -> IO Object

objectInitChipmunk _ space n@Nikki{} = Nikki.initChipmunk space n
objectInitChipmunk _ space tile@Tile{} = Tiles.initChipmunk space tile
objectInitChipmunk _ space tile@MergedTile{} = Tiles.initChipmunk space tile
objectInitChipmunk scene space robot@Robot{} = Robots.initChipmunk scene space robot
objectInitChipmunk _ space terminal@Terminal{} = Terminals.initChipmunk space terminal
objectInitChipmunk _ space o@MilkMachine{} = MilkMachine.initChipmunk space o
objectInitChipmunk _ space o@Box{} = Box.initChipmunk space o

-- objectInitChipmunk _space x = es "objectInitChipmunk" x

objectInitAnimation :: Object -> Object
objectInitAnimation o@Nikki{} = Nikki.initAnimation o
objectInitAnimation o@Tile{} = Tiles.initAnimation o
objectInitAnimation o@MergedTile{} = Tiles.initAnimation o
objectInitAnimation o@Terminal{} = Terminals.initAnimation o
objectInitAnimation o@Robot{} = Robots.initAnimation o
objectInitAnimation o@MilkMachine{} = MilkMachine.initAnimation o
objectInitAnimation o@Box{} = o
objectInitAnimation o@OSDObject{} = o
objectInitAnimation x = es "objectInitAnimation" x


-- * updating

updateObject :: Scene -> Seconds -> Collisions -> (Bool, ControlData)
    -> Object -> IO Object
updateObject scene now collisions cd n@Nikki{} =
    Nikki.update scene now collisions cd n
updateObject scene now collisions cd o@Tile{} =
    Tiles.update now o
updateObject scene now collisions cd o@Terminal{} =
    Terminals.update scene now cd o
updateObject scene now collisions cd o@Robot{} =
    Robots.update scene now collisions cd o
updateObject scene now collisions cd o@MilkMachine{} =
    return $ MilkMachine.update now o
updateObject scene now collisions cd object = return object


-- * rendering

render :: Ptr QPainter -> Qt.Position Double -> Scene -> Object -> IO ()
render painter offset s o@Terminal{} = Terminals.render painter offset s o
render ptr offset s tile@Tile{} = Tiles.render ptr offset tile
render ptr offset s tile@MergedTile{} = Tiles.render ptr offset tile
render ptr offset s n@Nikki{} = Nikki.render ptr offset n
render ptr offset s r@Robot{} = Robots.render ptr s offset r
render ptr offset s mm@MilkMachine{} = MilkMachine.render ptr offset mm
render ptr offset s o@Box{} = Box.render ptr offset o




