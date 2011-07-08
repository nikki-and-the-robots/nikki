{-# language ScopedTypeVariables #-}

module Base.Constants where


import Physics.Hipmunk

import Graphics.Qt

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

-- * Physics

tau :: Floating a => a
tau = 2 * pi

gravity :: CpFloat = 1975

-- there are multiple substeps (steps in chipmunk) per superstep.
subStepQuantum :: CpFloat = 0.002
superStepQuantum :: CpFloat = subStepQuantum * fromIntegral subStepsPerSuperStep
subStepsPerSuperStep :: Int = 8

-- | updates take place once per subStep
updateStepQuantum :: CpFloat = subStepQuantum

timeFactor :: CpFloat = 1

-- | how many digits to use for times
timeDigits :: Int
timeDigits = 2


-- * file directories

soundDir :: FilePath
soundDir = "sounds"

pngDir :: FilePath
pngDir = "png"

templateLevelsDir :: FilePath
templateLevelsDir = "templateLevels"


-- * misc

-- | initial window size, when not in fullscreen mode
programWindowSize = Windowed (Size 1024 640)

windowTitle :: String
windowTitle = "Nikki and the Robots"

goldenRatio :: Double
goldenRatio = (sqrt 5 + 1) / 2

levelEndDuration :: Double = 10
