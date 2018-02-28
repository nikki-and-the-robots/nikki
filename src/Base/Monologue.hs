
module Base.Monologue (Monologue, readSignMonologue) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import           System.FilePath

import           Base.Paths
import           Base.Prose
import           Utils

monologueDir = "monologues"

type Monologue = [Prose]

readSignMonologue :: FilePath -> IO Monologue
readSignMonologue name = do
    let dataPath = monologueDir </> name
    mPublicFile <- maybeExists =<< getDataFileName dataPath
    mStoryModeFile <- maybeExists =<< getStoryModeDataFileName dataPath

    case mPublicFile <|> mStoryModeFile of
        Just file -> parseMonologue <$> BS.toString <$> BS.readFile file
        Nothing -> return [pv ("monologue file not found: " ++ dataPath)]

parseMonologue :: String -> Monologue
parseMonologue =
    lines >>> map pVerbatim
