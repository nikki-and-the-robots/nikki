{-# language NamedFieldPuns #-}

module Build where


import Data.List

import System
import System.Directory


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

collectDependencies t = nubBy sameName $ allDeps t
  where
    allDeps Target{dependencies} = dependencies ++ concatMap allDeps dependencies

execute t = do
    putStrLn ("\tTarget: " ++ name t ++ "...")
    command t
    putStrLn ("\tTarget: " ++ name t ++ " done.")

sameName a b = name a == name b


-- * specific Types

data CabalOptions = CabalOptions {
    cabalOptions :: String,
    ghcOptions :: String
  }
    deriving Show


-- * Utils

-- | changes the working directory temporarily.
withCurrentDirectory :: FilePath -> IO () -> IO ()
withCurrentDirectory path cmd = do
    putStrLn ("Entering directory " ++ path)
    oldWorkingDirectory <- getCurrentDirectory
    setCurrentDirectory path
    x <- cmd
    setCurrentDirectory oldWorkingDirectory
    putStrLn ("Leaving directory " ++ path)
    return x

-- | executes a unix command on the shell and exits if it does not succeed.
trySystem :: String -> IO ()
trySystem cmd = do
    putStrLn ("Executing \"" ++ cmd ++ "\" ...")
    exitcode <- system cmd
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure n -> exitWith $ ExitFailure n
