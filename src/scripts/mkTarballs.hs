#!/usr/bin/env runhaskell


import Safe

import Control.Arrow

import System.Process
import System.Exit


main :: IO ()
main = do
    (ExitSuccess, stdout, stderr) <- readProcessWithExitCode "darcs" ["changes"] ""
    let tags = getTags stdout
        regexps = map toRegexp tags
    mapM_ (uncurry mkTarball) $ zip tags regexps

-- | Extracts tags from output of 'darcs changes'.
getTags :: String -> [String]
getTags =
    lines >>>
    map words >>>
    filter (headMay >>> (== Just "tagged")) >>>
    map tail >>>
    map unwords

-- | Converts a tag to a regexp matching (hopefully) only that one tag.
toRegexp :: String -> String
toRegexp =
    (\ t -> "\"^" ++ t ++ "$\"") >>>
    concatMap (\ c -> if c `elem` ['.', '(', ')'] then '\\' : c : [] else [c])

mkTarball :: String -> String -> IO ()
mkTarball tag regexp = do
    let cmd = "darcs dist --store-in-memory -t " ++ regexp ++
              " -d \"nikki-" ++ tag ++ "\""
    putStrLn cmd
    ExitSuccess <- system cmd
    return ()
