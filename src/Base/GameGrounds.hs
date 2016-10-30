{-# language DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Base.GameGrounds (
    GameGrounds(GameGrounds, gameMainLayerUpdatingRange),
    gameBackgrounds,
    gameMainLayer,
    gameForegrounds,
    GameLayer(GameLayer, gameXDistance, gameYDistance),
    gameContent,
    mkGameGrounds,
  ) where

import           Data.Accessor
import           Data.Data
import           Data.Indexable
import           Data.Indexable.Range

import           Base.Grounds

data GameGrounds a = GameGrounds {
    gameBackgrounds_ :: [GameLayer a],
    gameMainLayer_ :: Indexable a,
    gameMainLayerUpdatingRange :: Range,
    gameForegrounds_ :: [GameLayer a]
  }
    deriving (Show, Foldable, Data, Typeable)

gameBackgrounds :: Accessor (GameGrounds a) [GameLayer a]
gameBackgrounds = accessor gameBackgrounds_ (\ a r -> r{gameBackgrounds_ = a})

gameMainLayer :: Accessor (GameGrounds a) (Indexable a)
gameMainLayer = accessor gameMainLayer_ (\ a r -> r{gameMainLayer_ = a})

gameForegrounds :: Accessor (GameGrounds a) [GameLayer a]
gameForegrounds = accessor gameForegrounds_ (\ a r -> r{gameForegrounds_ = a})


data GameLayer a = GameLayer {
    gameContent_ :: [a],
    gameXDistance :: Double,
    gameYDistance :: Double
  }
    deriving (Show, Read, Data, Typeable, Foldable)

gameContent :: Accessor (GameLayer a) [a]
gameContent = accessor gameContent_ (\ a r -> r{gameContent_ = a})


-- * creation

mkGameGrounds :: Grounds o -> Range -> GameGrounds o
mkGameGrounds (Grounds backgrounds mainLayer foregrounds) updatingRange =
    GameGrounds
        (multi backgrounds)
        (mainLayer ^. content)
        updatingRange
        (multi foregrounds)
  where
    multi :: Indexable (Layer a) -> [GameLayer a]
    multi = fmap mkGameLayer . toList

mkGameLayer :: Layer a -> GameLayer a
mkGameLayer (Layer content xd yd) =
    GameLayer (toList content) xd yd
