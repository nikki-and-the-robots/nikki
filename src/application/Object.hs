
-- | does general stuff in the game
-- this includes:
--  - chipmunk initialisation
--  - updating
--  - chipmunk stepping
--  - rendering

module Object where


import Graphics.Qt as Qt

import Base.Events
import Base.Constants

import Game.Scene.Types

import Object.Types
import Object.Contacts


-- * updating

updateObject :: Scene -> Seconds -> Contacts -> (Bool, ControlData)
    -> Object_ -> IO Object_
updateObject scene seconds collisions cd object =
    update_ object seconds collisions cd


-- * rendering

render :: Ptr QPainter -> Offset -> Scene -> Object_ -> IO ()
-- render painter offset s o@Terminal{} = Terminals.render painter offset s o
-- render ptr offset s tile@Tile{} = Tiles.render ptr offset tile
-- render ptr offset s tile@MergedTile{} = Tiles.render ptr offset tile
-- render ptr offset s n@Nikki{} = Nikki.render ptr offset n
-- render ptr offset s r@Robot{} = Robots.render ptr s offset r
-- render ptr offset s mm@MilkMachine{} = MilkMachine.render ptr offset mm
-- render ptr offset s o@Box{} = Box.render ptr offset o
render ptr offset scene object = render_ object ptr offset



