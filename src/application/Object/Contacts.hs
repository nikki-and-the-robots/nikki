{-# language NamedFieldPuns #-}

module Object.Contacts (
    MyCollisionType(..),
    watchedContacts,

    Contacts(..),
    emptyContacts,
    nikkiTouchesTerminal,
  ) where


import Data.Set as Set

import Physics.Chipmunk hiding (position)

import Base.Types


data MyCollisionType
    = TileCT
    | NikkiBodyCT
    | NikkiFeetCT
    | TerminalCT
    | LaserCT
    | RobotCT
    | MilkMachineCT
    | BatteryCT
  deriving (Enum, Eq, Show)


-- empty in the sense that nothing collides
emptyContacts :: Contacts
emptyContacts = Contacts False False False empty empty


-- * setter (boolean to True)

setNikkiTouchesGround :: Contacts -> Contacts
setNikkiTouchesGround c = c{nikkiTouchesGround = True}

setNikkiTouchesLaser :: Contacts -> Contacts
setNikkiTouchesLaser c = c{nikkiTouchesLaser = True}

setNikkiTouchesMilkMachine :: Contacts -> Contacts
setNikkiTouchesMilkMachine c = c{nikkiTouchesMilkMachine = True}



watchedContacts :: [Callback MyCollisionType Contacts]
watchedContacts = [
    Callback (Watch TileCT NikkiFeetCT (\ _ _ -> setNikkiTouchesGround)) Solid,
    Callback (Watch RobotCT NikkiFeetCT (\ _ _ -> setNikkiTouchesGround)) Solid,

    Callback (Watch NikkiBodyCT TerminalCT addTerminal) Permeable,
    Callback (DontWatch TerminalCT NikkiFeetCT) Permeable,
    Callback (DontWatch RobotCT TerminalCT) Permeable,
    Callback (DontWatch TileCT TerminalCT) Permeable,

    batteryCallback NikkiBodyCT,
    batteryCallback NikkiFeetCT,
    Callback (DontWatch BatteryCT TerminalCT) Permeable,

    Callback (Watch NikkiBodyCT LaserCT (\ _ _ -> setNikkiTouchesLaser)) Permeable,
    Callback (Watch NikkiFeetCT LaserCT (\ _ _ -> setNikkiTouchesLaser)) Permeable,
    Callback (DontWatch RobotCT LaserCT) Permeable,

    Callback (Watch NikkiBodyCT MilkMachineCT (\ _ _ -> setNikkiTouchesMilkMachine)) Permeable,
    Callback (DontWatch NikkiFeetCT MilkMachineCT) Permeable,
    Callback (DontWatch RobotCT MilkMachineCT) Permeable
  ]
 where
    batteryCallback nikkiCT =
        Callback (Watch nikkiCT BatteryCT addBattery) Permeable



addTerminal :: Shape -> Shape -> Contacts -> Contacts
addTerminal _ terminalShape c@Contacts{terminals} =
    c{terminals = insert terminalShape terminals}


nikkiTouchesTerminal :: Contacts -> Bool
nikkiTouchesTerminal = not . Set.null . terminals




addBattery :: Shape -> Shape -> Contacts -> Contacts
addBattery _ batteryShape c =
    c{batteries = insert batteryShape (batteries c)}


