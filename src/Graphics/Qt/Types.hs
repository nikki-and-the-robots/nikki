{-# language DeriveDataTypeable, TypeSynonymInstances, EmptyDataDecls #-}

module Graphics.Qt.Types where

import Data.Generics
import Data.Binary
import Data.Abelian

import Utils


-- * Types

type QtInt = Int

type QtReal = Double -- unless running on ARM ;)

data QKeyEvent

-- * Position

data Position a = Position {positionX :: a, positionY :: a}
  deriving (Show, Read, Eq, Data, Typeable)

instance Abelian a => Abelian (Position a) where
    zero = Position zero zero
    (Position a b) +~ (Position x y) = Position (a +~ x) (b +~ y)
    (Position a b) -~ (Position x y) = Position (a -~ x) (b -~ y)

instance Functor Position where
    fmap f (Position a b) = Position (f a) (f b)

instance Binary a => Binary (Position a) where
    put (Position a b) = do
        put a
        put b
    get = do
        a <- get
        b <- get
        return $ Position a b

instance PP p => PP (Position p) where
    pp (Position x y) = unwords ["Position", pp x, pp y]


-- * Size

data Size a = Size {width :: a, height :: a}
  deriving (Eq, Show, Data, Typeable)

instance Functor Size where
    fmap f (Size a b) = Size (f a) (f b)

instance Abelian a => Abelian (Size a) where
    zero = Size zero zero
    (Size a b) +~ (Size x y) = Size (a +~ x) (b +~ y)
    (Size a b) -~ (Size x y) = Size (a -~ x) (b -~ y)

instance PP p => PP (Size p) where
    pp (Size x y) = unwords ["Size", pp x, pp y]

data Color = QtColor {
    redComponent :: QtInt,
    greenComponent :: QtInt,
    blueComponent :: QtInt,
    alphaComponent :: QtInt
  }
    deriving (Eq, Ord, Show)


-- * utils

sizeToPosition :: Size a -> Position a
sizeToPosition (Size x y) = Position x y

positionToSize :: Position a -> Size a
positionToSize (Position x y) = Size x y
