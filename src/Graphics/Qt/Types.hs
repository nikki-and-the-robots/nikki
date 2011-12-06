{-# language EmptyDataDecls, DeriveDataTypeable, DeriveFunctor #-}

module Graphics.Qt.Types where

import Data.Generics
import Data.Binary
import Data.Abelian
import Data.Accessor

import Control.DeepSeq

import System.Random

import Utils


-- * Types

type QtInt = Int

type QtReal = Double -- unless running on ARM ;)

data QKeyEvent

-- * Position

data Position a = Position {positionX :: a, positionY :: a}
  deriving (Show, Read, Eq, Ord, Data, Typeable, Functor)

x_, y_ :: Accessor (Position a) a
x_ = accessor positionX (\ a r -> r{positionX = a})
y_ = accessor positionY (\ a r -> r{positionY = a})

instance Abelian a => Abelian (Position a) where
    zero = Position zero zero
    (Position a b) +~ (Position x y) = Position (a +~ x) (b +~ y)
    (Position a b) -~ (Position x y) = Position (a -~ x) (b -~ y)

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

instance NFData a => NFData (Position a) where
    rnf (Position a b) = rnf a `seq` rnf b

instance VectorLike Position where
    componentWise op (Position a b) (Position x y) =
        Position (op a x) (op b y)
    split a = Position a a

rotatePosition :: Floating a => a -> Position a -> Position a
rotatePosition angle (Position x y) =
    Position x' y'
  where
    x' = cos angle * x - sin angle * y
    y' = sin angle * x + cos angle * y


-- * Size

data Size a = Size {width :: a, height :: a}
  deriving (Eq, Ord, Show, Data, Typeable, Functor)

width_, height_ :: Accessor (Size a) a
width_ = accessor width (\ a r -> r{width = a})
height_ = accessor height (\ a r -> r{height = a})

instance Abelian a => Abelian (Size a) where
    zero = Size zero zero
    (Size a b) +~ (Size x y) = Size (a +~ x) (b +~ y)
    (Size a b) -~ (Size x y) = Size (a -~ x) (b -~ y)

instance PP p => PP (Size p) where
    pp (Size x y) = unwords ["Size", pp x, pp y]

instance NFData a => NFData (Size a) where
    rnf (Size a b) = rnf a `seq` rnf b

instance VectorLike Size where
    componentWise op (Size a b) (Size x y) =
        Size (op a x) (op b y)
    split a = Size a a

data Color = QtColor {
    _red :: QtInt,
    _green :: QtInt,
    _blue :: QtInt,
    _alpha :: QtInt
  }
    deriving (Eq, Ord, Show)

alpha :: Accessor Color Double
alpha = accessor (byteIntToDouble . _alpha) (\ a r -> r{_alpha = doubleToByteInt a})

instance PP Color where
    pp (QtColor r g b a) = "Color: " ++ pp (r, g, b, a)

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

-- | Returns a somewhat random, but bright and opaque color.
semiRandomColor :: IO Color
semiRandomColor = do
    let rand = randomRIO (127, 255)
    r <- rand
    g <- rand
    b <- rand
    return $ QtColor r g b 255


-- * utils

size2position :: Size a -> Position a
size2position (Size x y) = Position x y

position2size :: Position a -> Size a
position2size (Position x y) = Size x y
