{-# language DeriveDataTypeable, TypeSynonymInstances #-}

module Graphics.Qt.Types where

import Data.Generics
import Data.Binary
import Data.Abelian


-- * Types

type QtInt = Int

type QtReal = Double -- unless running on ARM ;)

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

data Size a = Size {width :: a, height :: a}
  deriving (Eq, Show, Data, Typeable)

instance Functor Size where
    fmap f (Size a b) = Size (f a) (f b)

instance Abelian a => Abelian (Size a) where
    zero = Size zero zero
    (Size a b) +~ (Size x y) = Size (a +~ x) (b +~ y)
    (Size a b) -~ (Size x y) = Size (a -~ x) (b -~ y)

data Color = QtColor QtInt QtInt QtInt QtInt


-- * utils

-- | calculates a factor. Transform the second given size
-- by this factor and it will fit into the first given.
-- The seconds returned value is the offset of the inner box
-- from the outer box.
squeezeScaling :: Size QtReal -> Size QtReal -> (QtReal, Position QtReal)
squeezeScaling limits box =
    (scaling, offset)
  where
    scaling = min widthFactor heightFactor
    offset = Position
                ((width limits - width box * scaling) / 2)
                ((height limits - height box * scaling) / 2)
    widthFactor = width limits / width box
    heightFactor = height limits / height box

sizeToPosition :: Size a -> Position a
sizeToPosition (Size x y) = Position x y


