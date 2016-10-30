{-# language ScopedTypeVariables #-}

module Graphics.Qt.Colors where

import           System.Random

import           Graphics.Qt.Types

black :: Color = opaqueColor 0 0 0
white :: Color = opaqueColor 1 1 1
red :: Color = opaqueColor 1 0 0
green :: Color = opaqueColor 0 1 0
blue :: Color = opaqueColor 0 0 1
yellow :: Color = opaqueColor 1 1 0
magenta :: Color = opaqueColor 1 0 1
cyan :: Color = opaqueColor 0 1 1

signalRed :: Color = opaqueColor 1 0.216 0.216
lightYellow :: Color = opaqueColor 1 1 0.216
pink :: Color = opaqueColor 1 0.196 0.588
orange :: Color = opaqueColor 1 0.5 0
lightBlue :: Color = opaqueColor 0.28 0.63 0.79
lightGreen :: Color = opaqueColor 0.66 1 0.66
darkGrey :: Color = QtColor 64 64 64 255
turquoise :: Color = QtColor 72 214 242 255

transparent :: Color = QtColor 0 0 0 0


randomColor :: IO Color
randomColor =
    QtColor <$> randByte <*> randByte <*> randByte <*> randByte
  where
    randByte = randomRIO (0, 255)
