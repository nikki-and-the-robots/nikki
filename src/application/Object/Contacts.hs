{-# language NamedFieldPuns #-}

module Object.Contacts (
    MyCollisionType(..),
    watchedContacts,

    Contacts(..),
    nikkiTouchesTerminal,
  ) where


import Data.Set as Set hiding (map)
import Data.Array.Storable
import Data.Initial

import Physics.Chipmunk hiding (position)

import Base.Types


data MyCollisionType
    = TileCT
    | NikkiBodyCT
    | NikkiFeetCT
    | TerminalCT
    | LaserCT
    | RobotCT
    | TriggerCT
    | BatteryCT
    | FallingTileCT
  deriving (Enum, Eq, Show)


-- initial in the sense that nothing collides
instance Initial Contacts where
    initial = Contacts [] False False empty empty empty empty


-- * setter (boolean to True)

addNikkiContacts :: StorableArray Int Contact -> Double -> Contacts -> Contacts
addNikkiContacts contactArray coefficient c =
    c{nikkiContacts = ((contactArray, coefficient) : nikkiContacts c)}

setNikkiFeetTouchGround :: Contacts -> Contacts
setNikkiFeetTouchGround c = c{nikkiFeetTouchGround = True}

setNikkiTouchesLaser :: Contacts -> Contacts
setNikkiTouchesLaser c = c{nikkiTouchesLaser = True}

addTrigger :: Shape -> Contacts -> Contacts
addTrigger s c = c{triggers = insert s (triggers c)}

addTerminal :: Shape -> Contacts -> Contacts
addTerminal terminalShape c@Contacts{terminals} =
    c{terminals = insert terminalShape terminals}

nikkiTouchesTerminal :: Contacts -> Bool
nikkiTouchesTerminal = not . Set.null . terminals

addBattery :: Shape -> Contacts -> Contacts
addBattery batteryShape c =
    c{batteries = insert batteryShape (batteries c)}

addFallingTileContact :: Shape -> Contacts -> Contacts
addFallingTileContact fallingTileShape contacts =
    contacts{fallingTiles = insert fallingTileShape (fallingTiles contacts)}



watchedContacts :: [Callback MyCollisionType Contacts]
watchedContacts = [
    -- normal contacts of nikki
    Callback (FullWatch TileCT NikkiBodyCT (\ _ _ -> addNikkiContacts)) Solid,
    Callback (FullWatch TileCT NikkiFeetCT 
                (\ _ _ a b -> setNikkiFeetTouchGround . addNikkiContacts a b)) Solid,
    Callback (FullWatch RobotCT NikkiBodyCT (\ _ _ -> addNikkiContacts)) Solid,
    Callback (FullWatch RobotCT NikkiFeetCT
                (\ _ _ a b -> setNikkiFeetTouchGround . addNikkiContacts a b)) Solid,

    -- a trigger (in w switch) is activated
    Callback (Watch TileCT TriggerCT (\ _ t -> addTrigger t)) Permeable,

    -- nikki stands in front of a terminal 
    Callback (Watch NikkiBodyCT TerminalCT (\ _ t -> addTerminal t)) Permeable,
    Callback (DontWatch TerminalCT NikkiFeetCT) Permeable,
    -- robots and Tiles vs. Terminals
    Callback (DontWatch RobotCT TerminalCT) Permeable,
    Callback (DontWatch TileCT TerminalCT) Permeable,

    -- batteries
    batteryCallback NikkiBodyCT,
    batteryCallback NikkiFeetCT,
    Callback (DontWatch BatteryCT TerminalCT) Permeable,

    -- contact with nikki and falling tiles
    Callback (FullWatch FallingTileCT NikkiFeetCT
        (\ a b contacts normal -> addFallingTileContact a . addNikkiContacts contacts normal)) Solid

    -- Lasers
--     Callback (Watch NikkiBodyCT LaserCT (\ _ _ -> setNikkiTouchesLaser)) Permeable,
--     Callback (Watch NikkiFeetCT LaserCT (\ _ _ -> setNikkiTouchesLaser)) Permeable,
--     Callback (DontWatch RobotCT LaserCT) Permeable,

--     Callback (Watch NikkiBodyCT MilkMachineCT (\ _ _ -> setNikkiTouchesMilkMachine)) Permeable,
--     Callback (DontWatch NikkiFeetCT MilkMachineCT) Permeable,
--     Callback (DontWatch RobotCT MilkMachineCT) Permeable,

  ]
 where
    batteryCallback nikkiCT =
        Callback (Watch nikkiCT BatteryCT (\ _ b -> addBattery b)) Permeable




