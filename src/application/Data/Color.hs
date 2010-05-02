
module Data.Color where

data RGBA = RGBA {
    redC :: Double,
    greenC :: Double,
    blueC :: Double,
    alphaC :: Double
  }
    deriving Show

blue :: RGBA
blue = RGBA 0 0 1 1

yellow :: RGBA
yellow = RGBA 1 1 0 1

pink :: RGBA
pink = RGBA 1 0.196 0.588 1

orange :: RGBA
orange = RGBA 1 0.5 0 1

