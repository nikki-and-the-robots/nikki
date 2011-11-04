{-# language NamedFieldPuns, DeriveDataTypeable #-}

module Base.Grounds where


import Safe

import Data.Indexable as I
import Data.Generics
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, traverse)
import Data.Initial
import Data.Accessor

import Graphics.Qt

import Utils


-- | Multiple backgrounds, one main Layer, multiple foregrounds
-- if backgrounds (or foregrounds) have zero distance to the main layer,
-- the don't move wrt the main layer, but are rendered before (after)
-- the main layer.
data Grounds a = Grounds {
    backgrounds_ :: Indexable (Layer a),
    mainLayer_ :: Layer a,
    foregrounds_ :: Indexable (Layer a)
  }
    deriving (Show, Read, Data, Typeable)

backgrounds, foregrounds :: Accessor (Grounds a) (Indexable (Layer a))
backgrounds = accessor backgrounds_ (\ a r -> r{backgrounds_ = a})
foregrounds = accessor foregrounds_ (\ a r -> r{foregrounds_ = a})

mainLayer :: Accessor (Grounds a) (Layer a)
mainLayer = accessor mainLayer_ (\ a r -> r{mainLayer_ = a})

-- | one group of objects, positioned relatively to each other.
-- to be refined
data Layer a = Layer {
    content_ :: (Indexable a),
    xDistance :: Double,
    yDistance :: Double
  }
  deriving (Show, Read, Data, Typeable)

content :: Accessor (Layer a) (Indexable a)
content = accessor content_ (\ a r -> r{content_ = a})

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
    fmap f l@Layer{content_} = l{content_ = fmap f content_}

instance Foldable Layer where
    foldMap f l = foldMap f (l ^. content)

instance Traversable Layer where
    traverse cmd (Layer content x y) =
        {-# SCC "Base.Grounds.traverse" #-}
        Layer <$> traverse cmd content <*> pure x <*> pure y

instance Initial (Grounds a) where
    initial = Grounds initial initial initial

instance Initial (Layer a) where
    initial = Layer initial 1 1

-- * construction

mkMainLayer :: Indexable a -> Layer a
mkMainLayer c = content ^= c $ initial


-- * discriminators

isGroundsIndexOf :: GroundsIndex -> Grounds a -> Bool
isGroundsIndexOf MainLayer _ = True
isGroundsIndexOf (Backgrounds i) gs = isIndexOf i (gs ^. backgrounds)
isGroundsIndexOf (Foregrounds i) gs = isIndexOf i (gs ^. foregrounds)


-- * getter

mainLayerIndexable :: Grounds a -> Indexable a
mainLayerIndexable = (^. mainLayer .> content)

-- | access the indexed Layer
layerA :: GroundsIndex -> Accessor (Grounds a) (Layer a)
layerA (Backgrounds i) = backgrounds .> indexA i
layerA MainLayer = mainLayer
layerA (Foregrounds i) = foregrounds .> indexA i

-- | returns the number of contained Layers
-- PRE: isNormalized
numberOfLayers :: Grounds a -> Int
numberOfLayers (Grounds backgrounds _ foregrounds) =
    fromIntegral (I.length backgrounds + 1 + I.length foregrounds)

-- | returns all Layers below the selected (excluding the selected)
belowSelected :: GroundsIndex -> Grounds a -> [Layer a]
belowSelected index grounds =
    let bgs = grounds ^. backgrounds
        ml = grounds ^. mainLayer
        fgs = grounds ^. foregrounds
    in case index of
        Backgrounds i -> beforeIndex i bgs
        MainLayer -> I.toList bgs
        Foregrounds i -> I.toList bgs ++ [ml] ++ I.toList (beforeIndex i fgs)


-- * setter

setXDistance :: Double -> Layer a -> Layer a
setXDistance x l = l{xDistance = x}

setYDistance :: Double -> Layer a -> Layer a
setYDistance y l = l{yDistance = y}


-- * modifications

-- | returns an index referring to the next element
nextGroundsIndex :: Grounds a -> GroundsIndex -> GroundsIndex
nextGroundsIndex gs i = case i of
    Backgrounds i -> maybe MainLayer Backgrounds (nextIndex (gs ^. backgrounds) i)
    MainLayer -> firstForegroundsIndex
    Foregrounds i -> maybe firstBackgroundsIndex Foregrounds (nextIndex (gs ^. foregrounds) i)
  where
    firstForegroundsIndex = case keys (gs ^. foregrounds) of
        (a : _) -> Foregrounds a
        [] -> firstBackgroundsIndex
    firstBackgroundsIndex = case keys (gs ^. backgrounds) of
        (a : _) -> Backgrounds a
        [] -> MainLayer

previousGroundsIndex :: Grounds a -> GroundsIndex -> GroundsIndex
previousGroundsIndex gs i = case i of
    Backgrounds i -> maybe lastForegroundsIndex Backgrounds (previousIndex (gs ^. backgrounds) i)
    MainLayer -> lastBackgroundsIndex
    Foregrounds i -> maybe MainLayer Foregrounds (previousIndex (gs ^. foregrounds) i)
  where
    lastForegroundsIndex = maybe MainLayer Foregrounds (lastMay $ keys (gs ^. foregrounds))
    lastBackgroundsIndex = maybe lastForegroundsIndex Backgrounds (lastMay $ keys (gs ^. backgrounds))

-- | modifies the content of one Layer
modifyContent :: (Indexable a -> Indexable b) -> Layer a -> Layer b
modifyContent f l@Layer{content_} = l{content_ = f content_}


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
