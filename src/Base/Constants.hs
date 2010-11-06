{-# language ScopedTypeVariables #-}

module Base.Constants where


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

robotIdleEyeTime :: Double 
robotIdleEyeTime = 0.4

-- * Physics

gravity :: Double
gravity = 1904

stepQuantum :: Double
stepQuantum = 0.002

-- * file directories

soundDir :: FilePath
soundDir = "sounds"

pngDir :: FilePath
pngDir = "png"

templateLevelsDir :: FilePath
templateLevelsDir = "template_levels"

levelDir :: FilePath
levelDir = "levels"


-- * misc

windowTitle :: String
windowTitle = "Nikki and the Robots"

goldenRatio :: Double
goldenRatio = (sqrt 5 + 1) / 2

