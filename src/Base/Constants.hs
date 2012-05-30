{-# language ScopedTypeVariables #-}

module Base.Constants where


import Physics.Hipmunk

import Graphics.Qt


type Seconds = CpFloat

-- * menu

afterLevelWaitTime :: Seconds = 0.5 --1.5


-- * Graphics

-- | converts uberpixels to pixels
fromUber :: Num n => n -> n
fromUber = (* 4)

fromSquaredUber :: Num n => n -> n
fromSquaredUber = (* 16)

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


-- * sounds

globalSoundVolume, globalMusicVolume :: Float
globalSoundVolume = 0.7
globalMusicVolume = 0.6

-- * file directories

soundDir :: FilePath
soundDir = "sounds"

pngDir :: FilePath
pngDir = "png"

templateLevelsDir :: FilePath
templateLevelsDir = "templateLevels"


-- * misc

-- | initial window size, when not in fullscreen mode
defaultWindowSize :: Size Int
defaultWindowSize = Size 1024 640

swapInterval :: Int = 1

windowTitle :: String
windowTitle = "Nikki and the Robots"

goldenRatio :: Double
goldenRatio = (sqrt 5 + 1) / 2

levelEndDuration :: Double = 10

initialDebuggingSignals :: [Key]
initialDebuggingSignals =
--     play 1 ++
    []
  where
    edit n = DownArrow : DownArrow : DownArrow : Ctrl :
             replicate n DownArrow ++ Ctrl : []
    play n = DownArrow : Ctrl : Ctrl :
             replicate (n - 1) DownArrow ++ Ctrl : []


-- * development

useNonPublicSorts :: Bool = False
