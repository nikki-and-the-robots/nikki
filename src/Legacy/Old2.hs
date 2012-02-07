{-# language GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances,
    DeriveDataTypeable, DeriveFunctor, DeriveFoldable, TypeSynonymInstances #-}

-- | version of the save type from 2011-03-25

module Legacy.Old2 where


import Data.Data
import qualified Data.IntMap
import Data.Convertable
import Data.Foldable


import qualified Base.Types
import qualified Editor.Pickle.Types as Newer


type SaveType = Grounds PickleObject

data Grounds a = Grounds {
    backgrounds :: Indexable (Layer a),
    mainLayer :: Layer a,
    foregrounds :: Indexable (Layer a)
  }
    deriving (Show, Read, Data, Typeable)

instance Convertable SaveType (Newer.SaveType) where
    convert (Grounds bgs ml fgs) =
        Newer.PGrounds_1
            (convert $ toList bgs)
            (convert $ content ml)
            (convert $ toList fgs)

data Indexable a = Indexable {
    values :: Data.IntMap.IntMap a,
    keys :: [Index]
  }
    deriving (Show, Read, Data, Typeable, Foldable)

instance Convertable (Indexable PickleObject) [(Int, Newer.PObject)] where
    convert (Indexable values keys) =
        fmap (\ k -> (index k, convert (values Data.IntMap.! index k))) keys

newtype Index = Index {index :: Int}
  deriving (Show, Read, Enum, Num, Eq, Integral, Real, Ord, Data, Typeable)

data Layer a = Layer {
    content :: (Indexable a),
    xDistance :: Double,
    yDistance :: Double
  }
  deriving (Show, Read, Data, Typeable)

instance Convertable (Layer PickleObject) Newer.PLayer where
    convert (Layer content xd yd) =
        Newer.PLayer_1 (convert $ toList content) xd yd

data PickleObject = PickleObject {
    pickleSortId :: SortId,
    picklePosition :: EditorPosition,
    pickleOEMState :: Maybe String
  }
    deriving (Read, Show)

instance Convertable PickleObject Newer.PObject where
    convert (PickleObject sortId pos oemState) =
        Newer.PObject_1 (convert sortId) (convert pos) oemState

newtype SortId = SortId {getSortId :: FilePath}
  deriving (Show, Read, Eq)

instance Convertable SortId Base.Types.SortId where
    convert (SortId x) = Base.Types.SortId x

data EditorPosition = EditorPosition {
    editorX :: Double,
    editorY :: Double
  }
  deriving (Show, Read, Eq, Typeable, Data)

instance Convertable EditorPosition Base.Types.EditorPosition where
    convert (EditorPosition x y) =
        Base.Types.EditorPosition x y
