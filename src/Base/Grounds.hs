{-# language DeriveDataTypeable, NamedFieldPuns, ViewPatterns #-}

module Base.Grounds where


import Utils

import Data.Indexable as I
import Data.Generics
import Data.List
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, traverse)
import Data.Initial
import Data.Accessor

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

backgroundsA, foregroundsA :: Accessor (Grounds a) (Indexable (Layer a))
backgroundsA = accessor backgrounds (\ a r -> r{backgrounds = a})
foregroundsA = accessor foregrounds (\ a r -> r{foregrounds = a})

mainLayerA :: Accessor (Grounds a) (Layer a)
mainLayerA = accessor mainLayer (\ a r -> r{mainLayer = a})

-- | one group of objects, positioned relatively to each other.
-- to be refined
data Layer a = Layer {
    content :: (Indexable a),
    xDistance :: Double,
    yDistance :: Double
  }
  deriving (Show, Read, Data, Typeable)

contentA :: Accessor (Layer a) (Indexable a)
contentA = accessor content (\ a r -> r{content = a})

-- | Index for selecting one (Layer a) from a (Grounds a)
data GroundsIndex
    = Backgrounds Index
    | MainLayer
    | Foregrounds Index
  deriving Show


-- useful instances

instance Functor Grounds where
    fmap f (Grounds a b c) = Grounds (fmap (fmap f) a) (fmap f b) (fmap (fmap f) c)

instance Functor Layer where
    fmap f l@Layer{content} = l{content = fmap f content}

instance Foldable Layer where
    foldMap f l = foldMap f (l ^. contentA)

instance Traversable Layer where
    traverse cmd (Layer content x y) =
        Layer <$> traverse cmd content <*> pure x <*> pure y

instance Initial (Grounds a) where
    initial = Grounds initial initial initial

instance Initial (Layer a) where
    initial = Layer initial 1 1

-- * construction

mkMainLayer :: Indexable a -> Layer a
mkMainLayer c = contentA ^= c $ initial


-- * getter

mainLayerIndexable :: Grounds a -> Indexable a
mainLayerIndexable Grounds{mainLayer} = mainLayer ^. contentA

-- | access the indexed Layer
layerA :: GroundsIndex -> Accessor (Grounds a) (Layer a)
layerA (Backgrounds i) = backgroundsA .> indexA i
layerA MainLayer = mainLayerA
layerA (Foregrounds i) = foregroundsA .> indexA i

-- | extract the indexed Layer
(!||) :: Grounds a -> GroundsIndex -> Layer a
a !|| b = a ^. layerA b

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
        Backgrounds i -> genericTake i bgs
        MainLayer -> bgs
        Foregrounds i -> bgs ++ [ml] ++ genericTake i fgs


-- * setter

setXDistance :: Double -> Layer a -> Layer a
setXDistance x l = l{xDistance = x}

setYDistance :: Double -> Layer a -> Layer a
setYDistance y l = l{yDistance = y}


-- * conversions

-- | converts the GroundsIndex to an Int
-- PRE: isNormalized
toInt :: Grounds a -> GroundsIndex -> Int
toInt Grounds{} (Backgrounds i) =
    fromIntegral i
toInt Grounds{backgrounds} MainLayer =
    fromIntegral $ I.length backgrounds
toInt Grounds{backgrounds} (Foregrounds i) =
    fromIntegral (Index (I.length backgrounds) + 1 + i)

-- | reverse of toInt
-- PRE: isNormalized
fromInt :: Grounds a -> Int -> GroundsIndex
fromInt gs@(Grounds backgrounds _ foregrounds) i
    | i < 0
        = fromInt gs (fromIntegral i + numberOfLayers gs)
    | i < bgsLen
        = Backgrounds $ Index i
    | i == bgsLen
        = MainLayer
    | i < (I.length backgrounds + 1 + I.length foregrounds)
        = Foregrounds $ Index (i - bgsLen - 1)
    | i >= (I.length backgrounds + 1 + I.length foregrounds)
        = fromInt gs (fromIntegral i - numberOfLayers gs)
    | otherwise = e "fromInt"
  where bgsLen = I.length backgrounds


-- * modifications

-- | modifies the GroundIndex according to the given function
modifyGroundsIndex :: Grounds a -> (Int -> Int) -> GroundsIndex -> GroundsIndex
modifyGroundsIndex gs f i =
    fromInt gs (f (toInt gs i))

-- | modifies the content of one Layer
modifyContent :: (Indexable a -> Indexable b) -> Layer a -> Layer b
modifyContent f l@Layer{content} = l{content = f content}


-- * maps

layerMap :: (Layer a -> Layer b) -> Grounds a -> Grounds b
layerMap f (Grounds backgrounds mainLayer foregrounds) =
    Grounds (fmap f backgrounds) (f mainLayer) (fmap f foregrounds)

layerMapM_ :: Monad m => (Layer a -> m b) -> Grounds a -> m ()
layerMapM_ cmd (Grounds backgrounds mainLayer foregrounds) = do
    fmapM_ cmd backgrounds
    _ <- cmd mainLayer
    fmapM_ cmd foregrounds


-- * rendering offset

-- | calculates the offset for one Layer.
calculateLayerOffset :: Size Double -> Position Double -> (Double, Double) -> Position Double
calculateLayerOffset windowSize mainLayerOffset (xDistance, yDistance) =
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
