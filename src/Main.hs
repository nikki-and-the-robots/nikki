
module Main where


import Graphics.Qt

import qualified Top.Main


main = Top.Main.main initialSignals

initialSignals :: [Key]
initialSignals =
--     play 1 ++
    []
  where
    edit n = DownArrow : DownArrow : DownArrow : Ctrl :
             replicate n DownArrow ++ Ctrl : []
    play n = DownArrow : Ctrl :
             replicate (n - 1) DownArrow ++ Ctrl : []


