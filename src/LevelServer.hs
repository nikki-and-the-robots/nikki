

import Control.Applicative

import System.IO
import System.Console.CmdArgs
import System.Locale

import Utils.Scripting

import LevelServer.Networking
import LevelServer.Types



main = do
    hSetBuffering stdout NoBuffering
    putStrLn ("listening on port " ++ show port)
    options <- cmdArgs defaultOptions
    levelFiles <- getFiles (levelDir options) (Just ".nl")
    runServer $ serve options levelFiles

serve :: ServerOptions -> [FilePath] -> ClientToServer -> IO ServerToClient
serve options levelFiles GetLevelList =
    return $ LevelList $ map (baseURL options <//>) levelFiles

-- * server arguments

data ServerOptions = ServerOptions {
    levelDir :: FilePath,
    baseURL :: String -- url under which the given level directory is accessible from the net.
  }
    deriving (Typeable, Data)

defaultOptions = ServerOptions {
    levelDir = ""
        &= argPos 0
        &= typDir
        &= typ "LEVELDIR",
    baseURL = ""
        &= argPos 1
        &= typ "URL"
  }
    &= helpArg [explicit, name "h", name "help"]
