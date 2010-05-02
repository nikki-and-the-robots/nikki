
module Physics.Chipmunk.CollisionCallbacks (
    MyCollisionType(..),
    addMyCallback,
    setMyCollisionType,
  ) where

import Physics.Hipmunk


data MyCollisionType
    = TileCT
    | NikkiBodyCT
    | NikkiFeetCT
    | ActionCT
    | LaserCT
    | RobotCT
    | MilkMachineCT
  deriving (Enum, Eq, Show)

toNumber :: MyCollisionType -> CollisionType
toNumber = toEnum . fromEnum

addMyCallback :: Space -> (MyCollisionType, MyCollisionType) -> Callback -> IO ()
addMyCallback s (a, b) = addCallback s (toNumber a, toNumber b)

setMyCollisionType :: Shape -> MyCollisionType -> IO ()
setMyCollisionType s ct = setCollisionType s (toNumber ct)




