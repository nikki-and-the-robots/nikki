
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
import qualified Data.Vector
import Data.ByteString.Lazy (ByteString)

import Control.Arrow

import Physics.Chipmunk

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
    | PGrounds_2 { -- 0.3.3.0.90
        pbackgrounds :: [PLayer],
        physicsLayer :: [(Int, PObject)],
        pforegrounds :: [PLayer],
        pCachedTiles :: Maybe [ShapeType]
      }
  deriving (Show, Read)

type JSONString = ByteString

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

pickle :: DiskLevel -> SaveType
pickle (DiskLevel (Grounds bgs pl fgs) cachedTiles) =
    PGrounds_2
        (pickleMultiLayers bgs)
        pickledPl
        (pickleMultiLayers fgs)
        cachedTiles
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

-- | Levels to be saved and loaded to and from disk
data DiskLevel
    = DiskLevel
        (Grounds (EditorObject Sort_))
        CachedTiles

unpickle :: [Sort_] -> SaveType -> Either [Prose] DiskLevel
unpickle allSorts (PGrounds_2 bgs pl fgs cachedTiles) = do
    grounds <- Grounds <$>
        unpickleMultiLayers allSorts bgs <*>
        unpicklePhysicsLayer allSorts pl <*>
        unpickleMultiLayers allSorts fgs
    return $ DiskLevel grounds cachedTiles
unpickle allSorts (PGrounds_1 bgs pl fgs) = do
    grounds <- Grounds <$>
        unpickleMultiLayers allSorts bgs <*>
        unpicklePhysicsLayer allSorts pl <*>
        unpickleMultiLayers allSorts fgs
    let cachedTiles = Nothing
    return $ DiskLevel grounds cachedTiles

-- | unpickle layers (without preserving indices)
unpickleMultiLayers :: [Sort_] -> [PLayer] -> Either [Prose] (Indexable (Layer (EditorObject Sort_)))
unpickleMultiLayers allSorts =
    fmapM (unpickleMultiLayer allSorts) . fromList

unpickleMultiLayer :: [Sort_] -> PLayer -> Either [Prose] (Layer (EditorObject Sort_))
unpickleMultiLayer allSorts (PLayer_1 content xd yd) = do
    newContent <- fromList <$> fmapM (unpickleObject allSorts) content
    return $ Layer newContent xd yd

-- | unpickle physics layer (with indices)
unpicklePhysicsLayer :: [Sort_] -> [(Int, PObject)] -> Either [Prose] (Layer (EditorObject Sort_))
unpicklePhysicsLayer allSorts list = do
    content <- fmapM (unpickleObject allSorts) ix
    return $ Layer content 1 1
  where
    ix :: Indexable PObject
    ix = Indexable $ Data.Vector.fromList (map (first Index) list)

unpickleObject :: [Sort_] -> PObject -> Either [Prose] (EditorObject Sort_)
unpickleObject allSorts (PObject_1 id position oemState) =
    case Prelude.filter ((== id) . sortId) allSorts of
        [sort] -> return $ EditorObject sort position (fmap (unpickleOEM sort) oemState)
        [] -> Left (p "sort not found: " : pv (getSortId id) : [])
        _ -> Left (p "multiple sorts with the same id found: " : pv (getSortId id) : [])
