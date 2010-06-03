{-# language NamedFieldPuns #-}

module Build where


import Data.List

import System.Directory


import Utils


data Target
    = Target {
        name :: String,
        dependencies :: [Target],
        command :: IO ()
      }

addDependencies :: [Target] -> Target -> Target
addDependencies deps t = t{dependencies = deps ++ dependencies t}


makeTargetByName :: [Target] -> String -> IO ()
makeTargetByName allTargets name =
    case searchTargetByName allTargets name of
        Nothing -> fail $ targetNotFound allTargets name
        Just t -> makeTarget t

targetNotFound allTargets name_ = 
    "target not found: " ++ name_ ++ "\n"
    ++ "possible targets:\n"
    ++ unlines (map (('\t' :) . name) allTargets)

searchTargetByName :: [Target] -> String -> Maybe Target
searchTargetByName (a : r) n = if n == name a then Just a else searchTargetByName r n
searchTargetByName [] _ = Nothing


makeTarget :: Target -> IO ()
makeTarget t = do
    let deps = collectDependencies t
    mapM_ execute deps
    execute t

collectDependencies t = nubBy (withView name (==)) $ allDeps t
  where
    allDeps Target{dependencies} = dependencies ++ concatMap allDeps dependencies

execute t = do
    putStrLn ("\tTarget: " ++ name t ++ "...")
    command t
    putStrLn ("\tTarget: " ++ name t ++ " done.")



-- * specific Types

data CabalOptions = CabalOptions {
    cabalOptions :: String,
    ghcOptions :: String
  }
    deriving Show