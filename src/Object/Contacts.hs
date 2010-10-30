{-# language NamedFieldPuns #-}

module Object.Contacts (
    MyCollisionType(..),
    watchedContacts,

    Contacts(..),
    nikkiTouchesTerminal,
  ) where


import Data.Set as Set hiding (map)
import Data.Initial

import Physics.Chipmunk

import Base.Types


nikkiCollisionTypes = [NikkiFeetCT, NikkiBodyCT]

-- | collision types of objects that cause a collision (that are solid) (without Nikkis collision types)
solidCollisionTypes :: [MyCollisionType]
solidCollisionTypes =
  [
    TileCT,
    RobotCT,
    FallingTileCT
  ]


-- initial in the sense that nothing collides
instance Initial Contacts where
    initial = Contacts [] False False empty empty empty empty


-- * setter (boolean to True)

addNikkiContacts :: Collision -> Contacts -> Contacts
addNikkiContacts v c =
    c{nikkiContacts = (v : nikkiContacts c)}

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
watchedContacts =
    -- normal contacts of nikki
    concatMap nikkiSolidCallbacks solidCollisionTypes ++
    [switchCallback] ++
    nikkiTerminalCallbacks ++
    map terminalSolidCallback solidCollisionTypes ++
    map batteryCallback nikkiCollisionTypes ++
    [Callback (DontWatch BatteryCT TerminalCT) Permeable] ++
    nikkiFallingTilesCallbacks


nikkiSolidCallbacks solidCT = [
    Callback (FullWatch solidCT NikkiBodyCT (\ _ _ -> addNikkiContacts)) Solid,
    Callback (FullWatch solidCT NikkiFeetCT
                (\ _ _ v -> setNikkiFeetTouchGround . addNikkiContacts v)) Solid
  ]

-- nikki stands in front of a terminal 
nikkiTerminalCallbacks =
    map
        (\ nct -> Callback (Watch nct TerminalCT (\ _ t -> addTerminal t)) Permeable)
        nikkiCollisionTypes

batteryCallback nikkiCT =
    Callback (Watch nikkiCT BatteryCT (\ _ b -> addBattery b)) Permeable

-- a trigger (in a switch) is activated
switchCallback =
    Callback (Watch TileCT TriggerCT (\ _ t -> addTrigger t)) Permeable

terminalSolidCallback solidCT =
    Callback (DontWatch TerminalCT solidCT) Permeable

-- contact with nikki and falling tiles
nikkiFallingTilesCallbacks = [
    Callback (FullWatch FallingTileCT NikkiBodyCT
        (\ a b v -> addFallingTileContact a . addNikkiContacts v)) Solid,
    Callback (FullWatch FallingTileCT NikkiFeetCT
        (\ a b v -> addFallingTileContact a . setNikkiFeetTouchGround . addNikkiContacts v)) Solid
  ]


