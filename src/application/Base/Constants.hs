
module Base.Constants where


import System.FilePath


-- * Types

type Seconds = Double

-- * Application

windowWidth :: Int
windowWidth = 1000
windowHeight :: Int
windowHeight = 650

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
gravity = 1700 * time

-- how fast the physic works (all forces and accelerations times time)
time :: Double
time = 1

-- * file directories

soundDir = "data" </> "sounds"

pngDir :: FilePath
pngDir = "data" </> "png"

levelDir = "levels"



