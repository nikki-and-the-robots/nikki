

import System.IO

import LevelServer.Types
import LevelServer.Networking


main = do
    hSetBuffering stdout NoBuffering
    print =<< askServer GetLevelList
