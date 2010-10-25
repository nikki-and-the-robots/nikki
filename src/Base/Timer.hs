
module Base.Timer where


import Data.Time.Clock.POSIX

import Control.Applicative

import Utils


getNow :: IO Double
getNow = realToFrac <$> getPOSIXTime
