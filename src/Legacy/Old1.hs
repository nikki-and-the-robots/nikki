{-# language MultiParamTypeClasses, FlexibleInstances #-}

-- | version of the save type from 2010-06-24

module Legacy.Old1 (
    SaveType,
    convert,
  ) where


import Data.Convertable
import Data.Map as Map (Map, toList)
import Data.IntMap as IntMap (IntMap, fromList)
import qualified Data.Indexable

-- import Control.Arrow

import Utils

import qualified Sorts.Terminal

import qualified Legacy.Old2 as Newer



type SaveType = Grounds PickleObject

data Grounds a = Grounds {
    backgrounds :: Indexable (Layer a),
    mainLayer :: Layer a,
    foregrounds :: Indexable (Layer a)
  }
    deriving (Show, Read)

instance Convertable a b => Convertable (Grounds a) (Newer.Grounds b) where
    convert (Grounds a b c) = Newer.Grounds (convert a) (convert b) (convert c)

data Layer a = Layer {
    content :: (Indexable a),
    xDistance :: Double,
    yDistance :: Double,
    xVelocity :: Double,
    yVelocity :: Double,
    wrapped :: Bool
  }
  deriving (Show, Read)

instance Convertable a b => Convertable (Layer a) (Newer.Layer b) where
    convert (Layer a b c _d _e _f) =
        Newer.Layer (convert a) b c


data PickleObject = PickleObject {
    pickleSortId :: SortId,
    picklePosition :: EditorPosition,
    pickleOEMState :: Maybe String
  }
    deriving (Read, Show)

instance Convertable PickleObject Newer.PickleObject where
    convert (PickleObject a b c) =
        Newer.PickleObject (convert a) (convert b) (fmap convertOEMStates c)

newtype SortId = SortId FilePath
  deriving (Show, Read, Eq)

instance Convertable SortId Newer.SortId where
    convert (SortId a) = Newer.SortId a

data EditorPosition = EditorPosition Double Double
  deriving (Show, Read, Eq)

instance Convertable EditorPosition Newer.EditorPosition where
    convert (EditorPosition a b) = Newer.EditorPosition a b

data Indexable a = Indexable {
    values :: Map Index a,
    keys :: [Index]
  }
    deriving (Show, Read)

instance Convertable a b => Convertable (Indexable a) (Newer.Indexable b) where
    convert (Indexable a b) = Newer.Indexable (convert a) (convert b)

instance Convertable a b => Convertable (Map Index a) (IntMap b) where
    convert =
        Map.toList >>> map (\ (Index i, a) -> (i, convert a)) >>> IntMap.fromList

newtype Index = Index Int
  deriving (Show, Read, Eq, Ord)

instance Convertable Index Newer.Index where
    convert (Index i) = Newer.Index i

instance Convertable Index Data.Indexable.Index where
    convert (Index i) = Data.Indexable.Index i



convertOEMStates :: String -> String
convertOEMStates s =
    case readM s :: Maybe TerminalOEMState of
        Nothing -> s
        Just x -> show $ ((convert x) :: Sorts.Terminal.TerminalOEMState)


data TerminalOEMState
    = NoRobots
    | Robots {
        availableRobots :: [Index], -- INV: not null
        selectedRobot :: Index,
        attachedRobots :: [Index]
      }
  deriving (Read, Show)

instance Convertable TerminalOEMState Sorts.Terminal.TerminalOEMState where
    convert NoRobots = Sorts.Terminal.NoRobots
    convert (Robots a b c) =
        Sorts.Terminal.Robots (convert a) (convert b) (convert c)
