
module Editor.Pickle.MetaData (
    loadMetaData,
    saveMetaData,
  ) where


import Data.ByteString.Lazy as BSL
import Data.Aeson

import Text.Logging

import System.Directory
import System.FilePath

import Utils

import Base


metaFile :: FilePath -> FilePath
metaFile a = a <.> "meta"

loadMetaData :: FilePath -> IO LevelMetaData
loadMetaData levelFile = do
    exists <- doesFileExist (metaFile levelFile)
    if not exists then do
        logg Warning ("level meta data file does not exist: " ++ metaFile levelFile)
        return $ LevelMetaData (guessName levelFile) Nothing
      else do
        metaDataJSON :: BSL.ByteString <- io $ BSL.readFile (metaFile levelFile)
        let result :: Either [Prose] LevelMetaData =
                mapLeft (\ msg -> [p "error while parsing level meta data:", pv msg]) $
                decodeJSON metaDataJSON
        case result of
            Left msg -> do
                logg Warning ("meta data not parseable: " ++ levelFile)
                return $ LevelMetaData (guessName levelFile) Nothing
            Right x -> return x

saveMetaData :: FilePath -> LevelMetaData -> IO ()
saveMetaData file meta = BSL.writeFile (metaFile file) (encode meta)
