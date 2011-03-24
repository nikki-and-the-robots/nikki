{-# language DeriveDataTypeable #-}

module Base.RenderGrounds (
    RenderGrounds(..),
    physicsContentA,
    RenderLayer(..),
    mkRenderGrounds,
  ) where


import Data.Indexable
import Data.Data
import Data.Accessor
import Data.Foldable (Foldable, foldMap)
import Data.Traversable

import Base.Grounds

import Utils


data RenderGrounds a = RenderGrounds {
    renderBackgrounds :: [RenderLayer a],
    physicsContent :: Indexable a,
    renderForegrounds :: [RenderLayer a]
  }
    deriving (Show, Read, Data, Typeable)

physicsContentA :: Accessor (RenderGrounds a) (Indexable a)
physicsContentA = accessor physicsContent (\ a r -> r{physicsContent = a})

data RenderLayer a = RenderLayer {
    contentList :: [a],
    renderXDistance :: Double,
    renderYDistance :: Double
  }
    deriving (Show, Read, Data, Typeable)


-- * creation

mkRenderGrounds :: Grounds a -> RenderGrounds a
mkRenderGrounds (Grounds backgrounds mainlayer foregrounds) =
    RenderGrounds
        (multi backgrounds)
        (content mainlayer)
        (multi foregrounds)
  where
    multi :: Indexable (Layer a) -> [RenderLayer a]
    multi = fmap mkRenderLayer . toList

mkRenderLayer :: Layer a -> RenderLayer a
mkRenderLayer (Layer content xd yd) =
    RenderLayer (toList content) xd yd

-- * instances

instance Functor RenderLayer where
    fmap f (RenderLayer l x y) = RenderLayer (fmap f l) x y

instance Foldable RenderLayer where
    foldMap f (RenderLayer l x y) = foldMap f l

instance Traversable RenderLayer where
    traverse f (RenderLayer l x y) =
        RenderLayer <$> traverse f l <*> pure x <*> pure y
