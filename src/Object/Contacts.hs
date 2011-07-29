{-# language NamedFieldPuns #-}

module Object.Contacts (
    MyCollisionType(..),
    watchedContacts,

    Contacts(..),
    nikkiTouchesTerminal,
  ) where


import Data.Set as Set hiding (map, filter)
import Data.Initial
import Data.Abelian

import qualified Physics.Hipmunk as Hipmunk
import Physics.Chipmunk

import Utils

import Base


nikkiCollisionTypes = [NikkiLegsCT, NikkiHeadCT, NikkiLeftPawCT]

isSolidNikkiCollisionType NikkiLegsCT = True
isSolidNikkiCollisionType NikkiHeadCT = True
isSolidNikkiCollisionType _ = False

nikkiPermeability nct =
    if isSolidNikkiCollisionType nct
    then Solid
    else Permeable

-- | collision types of objects that cause a collision (that are solid) (without Nikkis collision types)
solidCollisionTypes :: [MyCollisionType]
solidCollisionTypes =
  [
    TileCT,
    RobotCT,
    FallingTileCT,
    BatteryCT
  ]


-- initial in the sense that nothing collides
instance Initial Contacts where
    initial = Contacts [] False empty empty empty empty Nothing


-- * setter (boolean to True)

addNikkiContacts :: Shape -> MyCollisionType -> Vector -> Contacts -> Contacts
addNikkiContacts s ct v c =
    c{nikkiCollisions = (NikkiCollision s (foldAngle $ toUpAngle v) ct : nikkiCollisions c)}

setNikkiTouchesDeadly :: Contacts -> Contacts
setNikkiTouchesDeadly c = c{nikkiTouchesDeadly = True}

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

-- | sets the sign, if it is closer to Nikki.
addSign :: Shape -> Shape -> Contacts -> IO Contacts
addSign nikkiShape signShape contacts = do
    nikkiPos <- get $ Hipmunk.position $ Hipmunk.body nikkiShape
    signPos <- get $ Hipmunk.position $ Hipmunk.body signShape
    let thisDistance = len (nikkiPos -~ signPos)
    return $ case nearestSign contacts of
        Nothing -> contacts{nearestSign = Just (signShape, thisDistance)}
        Just (_, otherDistance) ->
            if thisDistance < otherDistance then
                contacts{nearestSign = Just (signShape, thisDistance)}
              else
                contacts

-- | callbacks

watchedContacts :: [Callback MyCollisionType Contacts]
watchedContacts =
    -- normal contacts of nikki
    map (uncurry nikkiCallbacks) (cartesian (filter (/= BatteryCT) solidCollisionTypes) nikkiCollisionTypes) ++
    nikkiTerminalCallbacks ++
    map terminalSolidCallback solidCollisionTypes ++
    batteryCallbacks ++
    map nikkiFallingTilesCallbacks
        (filter isSolidNikkiCollisionType nikkiCollisionTypes) ++
    map signNikkiCallbacks (filter isSolidNikkiCollisionType nikkiCollisionTypes) ++
    map signPermeableCallbacks (solidCollisionTypes ++ filter (not . isSolidNikkiCollisionType) nikkiCollisionTypes) ++
    -- deadly solid (patrol robots)
    map deadlySolidNikki (filter isSolidNikkiCollisionType nikkiCollisionTypes) ++
    map deadlySolidPermeable (
        filter (not . isSolidNikkiCollisionType) nikkiCollisionTypes ++
        TerminalCT :
        SignCT :
        []) ++

    switchCallback :
    []


nikkiCallbacks solidCT nikkiCollisionType =
    Callback
        (FullWatch
            solidCT
            nikkiCollisionType
            (\ shape _ -> addNikkiContacts shape nikkiCollisionType))
        (nikkiPermeability nikkiCollisionType)

-- nikki stands in front of a terminal 
nikkiTerminalCallbacks =
    map (\ nct -> Callback (Watch nct TerminalCT (\ _ t -> return . addTerminal t)) Permeable)
        (filter isSolidNikkiCollisionType nikkiCollisionTypes) ++
    map (\ nct -> Callback (DontWatch nct TerminalCT) Permeable)
        (filter (not . isSolidNikkiCollisionType) nikkiCollisionTypes)

batteryCallbacks =
    map (\ nct -> Callback (Watch nct BatteryCT (\ _ b -> return . addBattery b)) Permeable)
        (filter isSolidNikkiCollisionType nikkiCollisionTypes) ++
    map (\ nct -> Callback (DontWatch nct BatteryCT) Permeable)
        (filter (not . isSolidNikkiCollisionType) nikkiCollisionTypes)

-- a trigger (in a switch) is activated
switchCallback =
    Callback (Watch TileCT TriggerCT (\ _ t -> return . addTrigger t)) Permeable

terminalSolidCallback solidCT =
    Callback (DontWatch TerminalCT solidCT) Permeable

-- contact with nikki and falling tiles
nikkiFallingTilesCallbacks nct =
    Callback (FullWatch FallingTileCT nct
        (\ a b v -> addFallingTileContact a . addNikkiContacts a nct v)) (nikkiPermeability nct)


-- * signs
signNikkiCallbacks :: MyCollisionType -> Callback MyCollisionType Contacts
signNikkiCallbacks nikkiCT =
    Callback (Watch nikkiCT SignCT (\ nikkiShape signShape -> addSign nikkiShape signShape)) Permeable

signPermeableCallbacks :: MyCollisionType -> Callback MyCollisionType Contacts
signPermeableCallbacks solidCT =
    Callback (DontWatch SignCT solidCT) Permeable

-- * deadly solid things

-- | add callbacks to let the level fail
deadlySolidNikki :: MyCollisionType -> Callback MyCollisionType Contacts
deadlySolidNikki nikkiCT =
    Callback (Watch DeadlySolidCT nikkiCT (\ _ _ -> return . setNikkiTouchesDeadly)) Solid

deadlySolidPermeable :: MyCollisionType -> Callback MyCollisionType Contacts
deadlySolidPermeable permeableCT =
    Callback (DontWatch DeadlySolidCT permeableCT) Permeable
