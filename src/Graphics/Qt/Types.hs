{-# language DeriveDataTypeable, TypeSynonymInstances, EmptyDataDecls #-}

module Graphics.Qt.Types where

import Data.Generics
import Data.Binary
import Data.Abelian
import Data.Accessor

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
    _red :: QtInt,
    _green :: QtInt,
    _blue :: QtInt,
    _alpha :: QtInt
  }
    deriving (Eq, Ord, Show)

alpha :: Accessor Color Double
alpha = accessor (byteIntToDouble . _alpha) (\ a r -> r{_alpha = doubleToByteInt a})

opaqueColor :: Double -> Double -> Double -> Color
opaqueColor r g b =
    QtColor
        (doubleToByteInt r)
        (doubleToByteInt g)
        (doubleToByteInt b)
        (doubleToByteInt 1)

doubleToByteInt :: Double -> QtInt
doubleToByteInt d | d >= 0 && d <= 1 =
    truncate (d * 255)

byteIntToDouble :: QtInt -> Double
byteIntToDouble x | x >= 0 && x <= 255 =
    fromIntegral x / 255


-- * utils

size2position :: Size a -> Position a
size2position (Size x y) = Position x y

position2size :: Position a -> Size a
position2size (Position x y) = Size x y
