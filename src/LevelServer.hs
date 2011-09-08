

import Control.Applicative

import System.IO
import System.Console.CmdArgs
import System.Locale
import System.Directory
import System.FilePath

import Network.URI

import Utils
import Utils.Scripting

import Base.Types.LevelMetaData

import LevelServer.Networking
import LevelServer.Types
import LevelServer.Configuration


main = do
    hSetBuffering stdout NoBuffering
    putStrLn ("listening on port " ++ show levelServerPort)
    options <- cmdArgs defaultOptions
    runServer $ serve options

serve :: ServerOptions -> ClientToServer -> IO ServerToClient
serve options GetLevelList = do
    levelFiles <- getFiles (levelDir options) (Just ".nl")
    return $ LevelList $
        map (escapeURIString isUnescapedInURI) $
        map (baseURL options <//>) levelFiles
serve options (UploadLevel meta level) = do
    let path = levelDir options </> meta_levelName meta <..> ".nl"
    exists <- doesFileExist path
    if exists then
        -- name clash
        return UploadNameClash
      else do
        writeFile path level
        saveMetaData path meta
        return UploadSucceeded
serve _ x = error ("NYI: " ++ show x)

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
