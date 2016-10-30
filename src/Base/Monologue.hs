
module Base.Monologue (Monologue, readSignMonologue) where

import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import           System.FilePath

import           Base.Paths
import           Base.Prose
import           Base.Types
import           Utils

monologueDir = "monologues"

type Monologue = [Prose]

readSignMonologue :: FilePath -> RM Monologue
readSignMonologue name = do
    let dataPath = monologueDir </> name
    mPublicFile <- io . maybeExists =<< getDataFileName dataPath
    mStoryModeFile <- join <$> io (fmapM maybeExists =<< getStoryModeDataFileName dataPath)

    io $ case mPublicFile <|> mStoryModeFile of
        Just file -> parseMonologue <$> BS.toString <$> BS.readFile file
        Nothing -> return [pv ("monologue file not found: " ++ dataPath)]

parseMonologue :: String -> Monologue
parseMonologue =
    lines >>> map pVerbatim
