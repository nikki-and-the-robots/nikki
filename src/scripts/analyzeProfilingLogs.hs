#!/usr/bin/env runghc

{-# language RecordWildCards #-}


import Text.Printf

import Control.Arrow

import System.Environment


main = do
    [file] <- getArgs
    c <- readFile file
    putStrLn $ showReport $ mkReport c

data Report = Report {
    slowDownAverage :: Double
  }

showReport :: Report -> String
showReport Report{..} = unlines (
    ("slowdown average: " ++ printf "%.2f%%" slowDownAverage) :
    [])

mkReport =
    lines >>>
    map read >>>
    avg >>>
    Report

deltas :: [Double] -> [Double]
deltas (a : b : r) = b - a : deltas (b : r)
deltas _ = []

avg :: [Double] -> Double
avg ll = sum ll / fromIntegral (length ll)
