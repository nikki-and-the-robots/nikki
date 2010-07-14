{-# language ScopedTypeVariables #-}

module Base.Constants where


import System.FilePath

import Graphics.Qt


-- * Types

type Seconds = Double

type Offset a = Position a

-- * Graphics

-- | converts uberpixels to pixels
fromUber :: Num n => n -> n
fromUber = (* 4)

-- | converts pixels to uberpixels
toUber :: Fractional n => n -> n
toUber = (/ 4)

-- | converts kachels (tiles) to pixels
fromKachel :: Num n => n -> n
fromKachel = (* 16) . fromUber

-- | converts pixels to kachels (tiles)
toKachel :: Fractional n => n -> n
toKachel = (/ 16) . toUber

-- * animation speeds

robotIdleEyeTime :: Double = 0.4

-- * Physics

gravity :: Double = 1904 * time

-- how fast the physic works (all forces and accelerations times time)
time :: Double = 1

stepQuantum :: Double = 0.002

-- * file directories

soundDir :: FilePath = "data" </> "sounds"

pngDir :: FilePath = "data" </> "png"

levelDir :: FilePath = "levels"


-- * misc

windowTitle = "Nikki and the Robots"

