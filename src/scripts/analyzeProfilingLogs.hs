#!/usr/bin/env runghc

{-# language RecordWildCards #-}


import Text.Printf

import Control.Arrow


main = do
    a <- readFile "physicsSlowDown.log"
    b <- readFile "fps.log"
    putStrLn $ showReport $ mkReport a b

data Report = Report {
    slowDownAverage :: Double,
    fpsAverage :: Double
  }

showReport :: Report -> String
showReport Report{..} = unlines (
    ("slowdown average: " ++ printf "%.2f%%" slowDownAverage) :
    ("FPS average: " ++ printf "%.2f" fpsAverage) :
    [])

mkReport physics fps =
    Report (inner physics) (inner fps)
  where
    inner =
        lines >>>
        map read >>>
        avg

deltas :: [Double] -> [Double]
deltas (a : b : r) = b - a : deltas (b : r)
deltas _ = []

avg :: [Double] -> Double
avg ll = sum ll / fromIntegral (length ll)
