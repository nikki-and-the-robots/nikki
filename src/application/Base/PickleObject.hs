{-# language DeriveDataTypeable #-}

module Base.PickleObject where


import Data.Indexable
import Data.Generics

import Graphics.Qt

import Base.Sprited


type EObject = EObject_ Sprited

type UnloadedEObject = EObject_ UnloadedSprited

data EObject_ sprited
    = ENikki {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
    | ETerminal {eObjectPosition :: (Position Double), eObjectSprited :: sprited, associatedRobots :: [Index]}
    | ERobot {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
    | EMilkMachine {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
    | ETile {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
    | EBox {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
  deriving (Show, Read, Eq, Data, Typeable)

type Offset = Position Double


-- * instances

instance Functor EObject_ where
    fmap f eo = eo{eObjectSprited = f (eObjectSprited eo)}


-- * discriminators

isETerminal :: EObject_ a -> Bool
isETerminal ETerminal{} = True
isETerminal _ = False

isERobot :: EObject -> Bool
isERobot ERobot{} = True
isERobot _ = False


setAssociatedRobots :: EObject -> [Index] -> EObject
setAssociatedRobots (ETerminal p s _) x = ETerminal p s x
