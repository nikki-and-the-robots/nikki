

-- | this is a script to generate the "data-files" section of the cabal file.

import Data.List

import System.FilePath

import Utils.Scripting

main :: IO ()
main = do
    dataFiles <- getFilesRecursive "../../data"
    putStrLn $ mkDataFilesSection dataFiles

mkDataFilesSection :: [FilePath] -> String
mkDataFilesSection files =
    "data-files:\n" ++
    "    " ++
        concat
            (intersperse ",\n    " (toWildCardFiles files)) ++
    "\n"

toWildCardFiles :: [FilePath] -> [FilePath]
toWildCardFiles = nub . map (\ f ->
    if null $ takeExtensions f
    then f
    else takeDirectory f </> "*" <.> takeExtensions f)
  where
    takeExtensions f = if null $ takeExtension f
        then ""
        else takeExtensions (dropExtension f) <.> takeExtension f
