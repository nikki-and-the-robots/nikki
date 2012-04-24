
-- | Adds battery counts to .meta files.
-- Command line arguments are level (.nl) files.
-- Has to be copied to the root of the repo and executed from there.


import Data.Initial
import Data.SelectTree

import Control.Monad.Reader
import Control.Monad.Error

import System.Environment
import System.FilePath

import Graphics.Qt

import Utils

import Base

import qualified Sorts.Battery

import Editor.Pickle
import Editor.Pickle.MetaData

import Top.Initialisation


main = withQApplication $ \ qApp -> do
    let config = defaultConfiguration initial
    flip runReaderT config $ withAllSorts $ \ sortTree -> liftIO $ do
        let allSorts = leafs sortTree
        mapM_ (addBatteryCount allSorts) =<< getArgs

addBatteryCount :: [Sort_] -> FilePath -> IO ()
addBatteryCount allSorts file = do
    numberOfBatteries <- countBatteries allSorts file
    setBatteries numberOfBatteries file

countBatteries :: [Sort_] -> FilePath -> IO Int
countBatteries allSorts levelFile = do
    eDiskLevel <- runErrorT $ loadByFilePath allSorts levelFile
    case eDiskLevel of
        Left proses -> error $ unlines $ map unP proses
        Right (DiskLevel grounds _) ->
            return $ Sorts.Battery.countBatteries $
                fmap editorSort $
                (grounds ^. mainLayer .> content)

setBatteries :: Int -> FilePath -> IO ()
setBatteries numberOfBatteries levelFile = do
    assertExistance (metaFile levelFile)
    old <- loadMetaData levelFile
    let new = old{meta_numberOfBatteries = Just numberOfBatteries}
    saveMetaData levelFile new
