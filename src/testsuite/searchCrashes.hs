

import Data.Char

import System.Environment
import System.Process
import System.Exit
import System.Posix.Directory

import Graphics.Qt
import Graphics.Qt.Events.Tests ()

import Utils

import Test.QuickCheck
import Test.QuickCheck.Property


main = do
    ["--please-do-random-things-to-my-userdata"] <- getArgs
    changeWorkingDirectory ".."
    quickCheck doesntCrash

doesntCrash :: [Key] -> Property
doesntCrash keys =
    morallyDubiousIOProperty $ do
        putStrLn ""
        print keys
        startNikki (keys +: CloseWindowKey)

mkInitialEventsOptions :: [Key] -> String
mkInitialEventsOptions = unwords . map (("-i " ++) . map toLower . show)

-- | starts the game, returning if False in case of a crash
startNikki :: [Key] -> IO Bool
startNikki events = do
    ec <- system ("./dist/build/core/core --run-in-place " ++ mkInitialEventsOptions events)
    return $ case ec of
        ExitSuccess -> True
        ExitFailure n -> False
