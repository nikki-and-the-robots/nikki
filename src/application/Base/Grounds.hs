{-# language DeriveDataTypeable, NamedFieldPuns, ViewPatterns #-}

module Base.Grounds where


import Utils

import Data.Indexable as I
import Data.Generics
import Data.List
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, traverse)

import Control.Applicative ((<*>), pure)

import Graphics.Qt


-- | Multiple backgrounds, one main Layer, multiple foregrounds
-- if backgrounds (or foregrounds) have zero distance to the main layer,
-- the don't move wrt the main layer, but are rendered before (after)
-- the main layer.
data Grounds a = Grounds {
    backgrounds :: Indexable (Layer a),
    mainLayer :: Layer a,
    foregrounds :: Indexable (Layer a)
  }
    deriving (Show, Read, Data, Typeable)

-- | one group of objects, positioned relatively to each other.
-- to be refined
data Layer a = Layer {
    content :: (Indexable a),
    xDistance :: Double,
    yDistance :: Double
  }
  deriving (Show, Read, Data, Typeable)

-- | Index for selecting one (Layer a) from a (Grounds a)
data GroundsIndex
    = BackGrounds Index
    | MainLayer
    | ForeGrounds Index
  deriving Show


-- useful instances

instance Functor Grounds where
    fmap f (Grounds a b c) = Grounds (fmap (fmap f) a) (fmap f b) (fmap (fmap f) c)

instance Functor Layer where
    fmap f l@Layer{content} = l{content = fmap f content}

instance Foldable Layer where
    foldMap f l = foldMap f (content l)

instance Traversable Layer where
    traverse cmd (Layer content x y) = Layer <$> traverse cmd content <*> pure x <*> pure y


-- * construction

emptyGrounds :: Grounds a
emptyGrounds = Grounds I.empty initialLayer I.empty

initialLayer :: Layer a
initialLayer = Layer I.empty 1 1

mkMainLayer :: Indexable a -> Layer a
mkMainLayer content = initialLayer{content}


-- * getter

mainLayerIndexable :: Grounds a -> Indexable a
mainLayerIndexable Grounds{mainLayer} = content mainLayer

-- | extracts the indexed Layer
(!||) :: Grounds a -> GroundsIndex -> Layer a
Grounds{backgrounds} !|| (BackGrounds i) = backgrounds !!! i
Grounds{mainLayer} !|| MainLayer = mainLayer
Grounds{foregrounds} !|| (ForeGrounds i) = foregrounds !!! i

-- | returns the number of contained Layers
-- PRE: isNormalized
numberOfLayers :: Grounds a -> Int
numberOfLayers (Grounds backgrounds _ foregrounds) =
    fromIntegral (I.length backgrounds + 1 + I.length foregrounds)

-- | returns all Layers below the selected (excluding the selected)
belowSelected :: GroundsIndex -> Grounds a -> [Layer a]
belowSelected index grounds =
    let bgs = I.toList $ backgrounds grounds
        ml = mainLayer grounds
        fgs = I.toList $ foregrounds grounds
    in case index of
        BackGrounds i -> genericTake i bgs
        MainLayer -> bgs
        ForeGrounds i -> bgs ++ [ml] ++ genericTake i fgs


-- * setter

setXDistance :: Layer a -> Double -> Layer a
setXDistance l x = l{xDistance = x}

setYDistance :: Layer a -> Double -> Layer a
setYDistance l y = l{yDistance = y}


-- * conversions

-- | converts the GroundsIndex to an Int
-- PRE: isNormalized
toInt :: Grounds a -> GroundsIndex -> Int
toInt Grounds{} (BackGrounds i) =
    fromIntegral i
toInt Grounds{backgrounds} MainLayer =
    fromIntegral $ I.length backgrounds
toInt Grounds{backgrounds} (ForeGrounds i) =
    fromIntegral (I.length backgrounds + 1 + i)

-- | reverse of toInt
-- PRE: isNormalized
fromInt :: Grounds a -> Int -> GroundsIndex
fromInt gs@(Grounds backgrounds _ foregrounds) (I.Index -> i)
    | i < 0
        = fromInt gs (fromIntegral i + numberOfLayers gs)
    | i < bgsLen
        = BackGrounds i
    | i == bgsLen
        = MainLayer
    | i < (I.length backgrounds + 1 + I.length foregrounds)
        = ForeGrounds (i - bgsLen - 1)
    | i >= (I.length backgrounds + 1 + I.length foregrounds)
        = fromInt gs (fromIntegral i - numberOfLayers gs)
    | otherwise = e "fromInt"
  where bgsLen = I.length backgrounds

-- * modifications

-- | modifies the main Layer
modifyMainLayer :: (Indexable a -> Indexable a) -> Grounds a -> Grounds a
modifyMainLayer f = modifySelectedLayer MainLayer (modifyContent f)

modifyMainLayerM :: Monad m => (Indexable a -> m (Indexable a)) -> Grounds a -> m (Grounds a)
modifyMainLayerM f (Grounds backgrounds mainLayer foregrounds) = do
    mainLayer' <- modifyContentM f mainLayer
    return $ Grounds backgrounds mainLayer' foregrounds


-- | modifies the Layer given by the GroundsIndex
modifySelectedLayer :: GroundsIndex -> (Layer a -> Layer a) -> Grounds a -> Grounds a
modifySelectedLayer (BackGrounds i) f (Grounds backgrounds mainLayer foregrounds) =
    Grounds (modifyByIndex f i backgrounds) mainLayer foregrounds
modifySelectedLayer MainLayer f (Grounds backgrounds mainLayer foregrounds) =
    Grounds backgrounds (f mainLayer) foregrounds
modifySelectedLayer (ForeGrounds i) f (Grounds backgrounds mainLayer foregrounds) =
    Grounds backgrounds mainLayer (modifyByIndex f i foregrounds)

-- | modifies the GroundIndex according to the given function
modifyGroundsIndex :: Grounds a -> (Int -> Int) -> GroundsIndex -> GroundsIndex
modifyGroundsIndex gs f i =
    fromInt gs (f (toInt gs i))

-- | modifies the content of one Layer
modifyContent :: (Indexable a -> Indexable b) -> Layer a -> Layer b
modifyContent f l@Layer{content} = l{content = f content}

modifyContentM :: Monad m => (Indexable a -> m (Indexable b)) -> Layer a -> m (Layer b)
modifyContentM f layer@Layer{content} = do
    content' <- f content
    return layer{content= content'}


-- * maps

layerMap :: (Layer a -> Layer b) -> Grounds a -> Grounds b
layerMap f (Grounds backgrounds mainLayer foregrounds) =
    Grounds (fmap f backgrounds) (f mainLayer) (fmap f foregrounds)

layerMapM_ :: Monad m => (Layer a -> m b) -> Grounds a -> m ()
layerMapM_ cmd (Grounds backgrounds mainLayer foregrounds) = do
    fmapM_ cmd backgrounds
    _ <- cmd mainLayer
    fmapM_ cmd foregrounds


-- * updating Layers

updateLayer :: Layer a -> Layer a
updateLayer = id

-- * rendering offset

-- | calculates the offset for one Layer.
calculateLayerOffset :: Size Double -> Position Double -> Layer a -> Position Double
calculateLayerOffset windowSize mainLayerOffset Layer{xDistance, yDistance} =
    Position xLayerOffset yLayerOffset
  where
    xLayerOffset = xMainOffset * xDistance + xWindowOffset * (1 - xDistance)
    yLayerOffset = yMainOffset * yDistance + yWindowOffset * (1 - yDistance)

    (Position xMainOffset yMainOffset) = mainLayerOffset
    (Size width height) = windowSize
    xWindowOffset = width / 2
    yWindowOffset = height / 2


-- | modify the distances of the second given Layer as if the first was the mainLayer
correctDistances :: Layer a -> Layer b -> Layer b
correctDistances pretendMain l@Layer{xDistance = oldX, yDistance = oldY} =
    l{
        xDistance = inner (xDistance pretendMain) oldX,
        yDistance = inner (yDistance pretendMain) oldY
      }
  where
    inner 0 _ = 1
    inner pretend old = old / pretend






