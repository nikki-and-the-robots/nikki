{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | version of the save type from 2010-06-24

module Editor.Pickle.Old1 (
    SaveType,
    convert,
  ) where


import Data.Convertable
import Data.Map as Map (Map, toList)
import Data.IntMap as IntMap (IntMap, fromList)

-- import Control.Arrow

import Utils

import qualified Sorts.Terminal

import qualified Editor.Pickle.Types as Newest



type SaveType = Grounds PickleObject

data Grounds a = Grounds {
    backgrounds :: Indexable (Layer a),
    mainLayer :: Layer a,
    foregrounds :: Indexable (Layer a)
  }
    deriving (Show, Read)

instance Convertable a b => Convertable (Grounds a) (Newest.Grounds b) where
    convert (Grounds a b c) = Newest.Grounds (convert a) (convert b) (convert c)

data Layer a = Layer {
    content :: (Indexable a),
    xDistance :: Double,
    yDistance :: Double,
    xVelocity :: Double,
    yVelocity :: Double,
    wrapped :: Bool
  }
  deriving (Show, Read)

instance Convertable a b => Convertable (Layer a) (Newest.Layer b) where
    convert (Layer a b c _d _e _f) =
        Newest.Layer (convert a) b c


data PickleObject = PickleObject {
    pickleSortId :: SortId,
    picklePosition :: EditorPosition,
    pickleOEMState :: Maybe String
  }
    deriving (Read, Show)

instance Convertable PickleObject Newest.PickleObject where
    convert (PickleObject a b c) =
        Newest.PickleObject (convert a) (convert b) (fmap convertOEMStates c)

newtype SortId = SortId FilePath
  deriving (Show, Read, Eq)

instance Convertable SortId Newest.SortId where
    convert (SortId a) = Newest.SortId a

data EditorPosition = EditorPosition Double Double
  deriving (Show, Read, Eq)

instance Convertable EditorPosition Newest.EditorPosition where
    convert (EditorPosition a b) = Newest.EditorPosition a b

data Indexable a = Indexable {
    values :: Map Index a,
    keys :: [Index]
  }
    deriving (Show, Read)

instance Convertable a b => Convertable (Indexable a) (Newest.Indexable b) where
    convert (Indexable a b) = Newest.Indexable (convert a) (convert b)

instance Convertable a b => Convertable (Map Index a) (IntMap b) where
    convert =
        Map.toList >>> map (\ (Index i, a) -> (i, convert a)) >>> IntMap.fromList

newtype Index = Index Int
  deriving (Show, Read, Eq, Ord)

instance Convertable Index Newest.Index where
    convert (Index i) = Newest.Index i




convertOEMStates :: String -> String
convertOEMStates s =
    case readM s :: Maybe TerminalOEMState of
        Nothing -> s
        Just x -> show $ ((convert x) :: Sorts.Terminal.OEMState)


data TerminalOEMState
    = NoRobots
    | Robots {
        availableRobots :: [Index], -- INV: not null
        selectedRobot :: Index,
        attachedRobots :: [Index]
      }
  deriving (Read, Show)

instance Convertable TerminalOEMState Sorts.Terminal.OEMState where
    convert NoRobots = Sorts.Terminal.NoRobots
    convert (Robots a b c) =
        Sorts.Terminal.Robots (convert a) (convert b) (convert c)




