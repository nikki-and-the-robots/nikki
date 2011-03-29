{-# language DeriveDataTypeable #-}

module Base.GameGrounds (
    GameGrounds(..),
    gameMainLayerA,
    GameLayer(..),
    mkGameGrounds,
  ) where


import Data.Indexable
import Data.Data
import Data.Accessor
import Data.Foldable (Foldable, foldMap)
import Data.Traversable

import Base.Grounds

import Utils


data GameGrounds a = GameGrounds {
    gameBackgrounds :: [GameLayer a],
    gameMainLayer :: Indexable a,
    gameForegrounds :: [GameLayer a]
  }
    deriving (Show, Read, Data, Typeable)

gameMainLayerA :: Accessor (GameGrounds a) (Indexable a)
gameMainLayerA = accessor gameMainLayer (\ a r -> r{gameMainLayer = a})

data GameLayer a = GameLayer {
    gameContent :: [a],
    gameXDistance :: Double,
    gameYDistance :: Double
  }
    deriving (Show, Read, Data, Typeable)


-- * creation

mkGameGrounds :: Grounds a -> GameGrounds a
mkGameGrounds (Grounds backgrounds mainlayer foregrounds) =
    GameGrounds
        (multi backgrounds)
        (content mainlayer)
        (multi foregrounds)
  where
    multi :: Indexable (Layer a) -> [GameLayer a]
    multi = fmap mkGameLayer . toList

mkGameLayer :: Layer a -> GameLayer a
mkGameLayer (Layer content xd yd) =
    GameLayer (toList content) xd yd

-- * instances

instance Functor GameLayer where
    fmap f (GameLayer l x y) = GameLayer (fmap f l) x y

instance Foldable GameLayer where
    foldMap f (GameLayer l x y) = foldMap f l

instance Traversable GameLayer where
    traverse f (GameLayer l x y) =
        GameLayer <$> traverse f l <*> pure x <*> pure y
