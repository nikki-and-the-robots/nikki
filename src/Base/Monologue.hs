
module Base.Monologue (Monologue, readSignMonologue) where


import Prelude hiding (catch)

import Control.Monad

import System.FilePath

import Utils

import Base.Prose
import Base.Paths
import Base.Types


monologueDir = "monologues"

type Monologue = [Prose]

readSignMonologue :: FilePath -> RM Monologue
readSignMonologue name = do
    let dataPath = monologueDir </> name
    mPublicFile <- io . maybeExists =<< getDataFileName dataPath
    mStoryModeFile <- join <$> io (fmapM maybeExists =<< getStoryModeDataFileName dataPath)

    io $ case mPublicFile <|> mStoryModeFile of
        Just file -> parseMonologue <$> io (readFile file)
        Nothing -> return [pv ("monologue file not found: " ++ dataPath)]

parseMonologue :: String -> Monologue
parseMonologue =
    lines >>> map pVerbatim
