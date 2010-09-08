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




nikkiCollisionTypes :: [MyCollisionType]
nikkiCollisionTypes = map NikkiCT allNikkiCollisionType

solidCollisionTypes :: [MyCollisionType]
solidCollisionTypes = [
    TileCT,
    RobotCT,
    FallingTileCT
  ]


-- initial in the sense that nothing collides
instance Initial Contacts where
    initial = Contacts [] False False False empty empty empty empty


-- * setter (boolean to True)

addNikkiContacts :: StorableArray Int Contact -> Double -> Contacts -> Contacts
addNikkiContacts contactArray coefficient c =
    c{nikkiContacts = ((contactArray, coefficient) : nikkiContacts c)}

setNikkiFeetTouchGround :: Contacts -> Contacts
setNikkiFeetTouchGround c = c{nikkiFeetTouchGround = True}

setNikkiPawsTouchesGround :: Contacts -> Contacts
setNikkiPawsTouchesGround c = c{nikkiPawTouchesGround = True}

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
watchedContacts =
    -- normal contacts of nikki
    concatMap nikkiSolidCallbacks solidCollisionTypes ++
    [switchCallback] ++
    nikkiTerminalCallbacks ++
    map terminalSolidCallback solidCollisionTypes ++
    map batteryCallback nikkiCollisionTypes ++
    [Callback (DontWatch BatteryCT TerminalCT) Permeable] ++
    nikkiFallingTilesCallbacks

    -- Lasers
--     Callback (Watch NikkiBodyCT LaserCT (\ _ _ -> setNikkiTouchesLaser)) Permeable,
--     Callback (Watch NikkiFeetCT LaserCT (\ _ _ -> setNikkiTouchesLaser)) Permeable,
--     Callback (DontWatch RobotCT LaserCT) Permeable,

--     Callback (Watch NikkiBodyCT MilkMachineCT (\ _ _ -> setNikkiTouchesMilkMachine)) Permeable,
--     Callback (DontWatch NikkiFeetCT MilkMachineCT) Permeable,
--     Callback (DontWatch RobotCT MilkMachineCT) Permeable,


nikkiSolidCallbacks solidCT = [
    Callback (FullWatch solidCT (NikkiCT NikkiHead) (\ _ _ -> addNikkiContacts)) Solid,
    Callback (FullWatch solidCT (NikkiCT NikkiFeet)
                (\ _ _ a b -> setNikkiFeetTouchGround . addNikkiContacts a b)) Solid,
    Callback (FullWatch solidCT (NikkiCT NikkiPaws)
                (\ _ _ a b -> setNikkiPawsTouchesGround . addNikkiContacts a b)) Solid
  ]

-- nikki stands in front of a terminal 
nikkiTerminalCallbacks = [
    Callback (Watch (NikkiCT NikkiHead) TerminalCT (\ _ t -> addTerminal t)) Permeable,
    Callback (DontWatch TerminalCT (NikkiCT NikkiFeet)) Permeable,
    Callback (DontWatch TerminalCT (NikkiCT NikkiPaws)) Permeable
  ]

batteryCallback nikkiCT =
    Callback (Watch nikkiCT BatteryCT (\ _ b -> addBattery b)) Permeable

-- a trigger (in a switch) is activated
switchCallback =
    Callback (Watch TileCT TriggerCT (\ _ t -> addTrigger t)) Permeable

terminalSolidCallback solidCT =
    Callback (DontWatch TerminalCT solidCT) Permeable

-- contact with nikki and falling tiles
nikkiFallingTilesCallbacks = [
    Callback (FullWatch FallingTileCT (NikkiCT NikkiHead) (\ a b c cn -> addFallingTileContact a . addNikkiContacts c cn)) Solid,
    Callback (FullWatch FallingTileCT (NikkiCT NikkiFeet)
                (\ a b c cn -> addFallingTileContact a . setNikkiFeetTouchGround . addNikkiContacts c cn)) Solid,
    Callback (FullWatch FallingTileCT (NikkiCT NikkiPaws)
                (\ a b c cn -> addFallingTileContact a . setNikkiPawsTouchesGround . addNikkiContacts c cn)) Solid
  ]


