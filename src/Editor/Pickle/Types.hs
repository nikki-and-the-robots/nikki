
-- | This module defines types and conversion functions
-- for the types used to save levels to files.
-- It employs versioned constructors to allow changes to the file
-- format.
-- If the data structures of the game change,
-- the conversion functions have to be adapted. If this isn't
-- possible with the existing SaveTypes, new versioned constructors
-- have to be added.

module Editor.Pickle.Types where


import Data.Indexable
import qualified Data.IntMap

import Control.Arrow

import Utils

import Base

import Object


-- | type used for level file content
-- type FileFormat = ByteString.ByteString
type FileFormat = String

-- | save type
data SaveType
    = PGrounds_1 {
        pbackgrounds :: [PLayer],
        physicsLayer :: [(Int, PObject)],
        pforegrounds :: [PLayer]
      }
  deriving (Show, Read)

data PLayer
    = PLayer_1 {
        pContent :: [PObject],
        pXDistance :: Double,
        pYDistance :: Double
      }
  deriving (Show, Read)

data PObject
    = PObject_1 {
        pSortId :: SortId,
        pPosition :: EditorPosition,
        pOEMState :: Maybe String
    }
  deriving (Read, Show)


-- * pickling

pickle :: Grounds (EditorObject Sort_) -> SaveType
pickle (Grounds bgs pl fgs) =
    PGrounds_1 (pickleMultiLayers bgs) pickledPl (pickleMultiLayers fgs)
  where
    pickledPl = map (\ k -> (index k, pickleObject ((pl ^. content) !!! k))) $
        keys (pl ^. content)

-- | pickle Layers (disregarding indices, both layer and object-wise)
pickleMultiLayers :: Indexable (Layer (EditorObject Sort_)) -> [PLayer]
pickleMultiLayers =
    fmap pickleMultiLayer . toList

-- | pickle a Layer (disregarding indices)
pickleMultiLayer :: Layer (EditorObject Sort_) -> PLayer
pickleMultiLayer (Layer content xd yd) =
    PLayer_1 (fmap pickleObject $ toList content) xd yd

pickleObject :: EditorObject Sort_ -> PObject
pickleObject (EditorObject sort pos oemState) =
    PObject_1 (sortId sort) pos (fmap oemPickle oemState)


-- * unpickling

unpickle :: [Sort_] -> SaveType -> Grounds (EditorObject Sort_)
unpickle allSorts (PGrounds_1 bgs pl fgs) =
    Grounds
        (unpickleMultiLayers allSorts bgs)
        (unpicklePhysicsLayer allSorts pl)
        (unpickleMultiLayers allSorts fgs)

-- | unpickle layers (without preserving indices)
unpickleMultiLayers :: [Sort_] -> [PLayer] -> Indexable (Layer (EditorObject Sort_))
unpickleMultiLayers allSorts =
    fmap (unpickleMultiLayer allSorts) . fromList

unpickleMultiLayer :: [Sort_] -> PLayer -> Layer (EditorObject Sort_)
unpickleMultiLayer allSorts (PLayer_1 content xd yd) =
    Layer (fromList $ fmap (unpickleObject allSorts) content) xd yd

-- | unpickle physics layer (with indices)
unpicklePhysicsLayer :: [Sort_] -> [(Int, PObject)] -> Layer (EditorObject Sort_)
unpicklePhysicsLayer allSorts list =
    Layer (fmap (unpickleObject allSorts) ix) 1 1
  where
    ix :: Indexable PObject
    ix = Indexable (map (first Index) list)

unpickleObject :: [Sort_] -> PObject -> EditorObject Sort_
unpickleObject allSorts (PObject_1 id position oemState) =
    EditorObject sort position (fmap (unpickleOEM sort) oemState)
  where
    sort = case Prelude.filter ((== id) . sortId) allSorts of
        [x] -> x
        [] -> error ("sort not found: " ++ getSortId id)
        _ -> error ("multiple sorts with the same id found: " ++ getSortId id)
