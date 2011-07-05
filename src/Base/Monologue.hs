
module Base.Monologue (Monologue, readStoryModeMonologue) where


import System.FilePath

import Utils

import Base.Prose
import Base.Paths


monologueDir = "monologues"

type Monologue = [Prose]

readStoryModeMonologue :: String -> IO Monologue
readStoryModeMonologue name = do
    mFile <- getStoryModeMonologueFile name
    case mFile of
        Just file -> parseMonologue <$> io (readFile file)
        Nothing -> return [pv ("monologue file not found: " ++ monologueDir </> name)]

getStoryModeMonologueFile :: String -> IO (Maybe FilePath)
getStoryModeMonologueFile name = do
    -- TODO: load monologues in different languages
    let file = monologueDir </> name
    io $ getStoryModeDataFileName file

parseMonologue :: String -> Monologue
parseMonologue =
    lines >>> map pVerbatim
