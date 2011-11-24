{-# language Rank2Types #-}

-- | Module for representing basic functions in one dimension
-- used with Size and Position.
-- Useful for abstracting algorithms from the dimension (component).

module Graphics.Qt.Dimension where


import Data.Accessor

import Graphics.Qt.Types


data Dimension = Dimension {
    posThis :: forall a . Accessor (Position a) a,
    posOther :: forall a . Accessor (Position a) a,
    sizeThis :: forall a . Accessor (Size a) a,
    sizeOther :: forall a . Accessor (Size a) a
  }

verticalDimension = Dimension {
    posThis = y_,
    posOther = x_,
    sizeThis = height_,
    sizeOther = width_
  }

horizontalDimension = Dimension {
    posThis = x_,
    posOther = y_,
    sizeThis = width_,
    sizeOther = height_
  }
