
module Base.Types.LevelFile (
    LevelFile(levelFilePath),
    standardLevel,
    userLevel,
    templateLevel,
    unknownLevel,
    isUserLevel,
    isTemplateLevel,
    levelName,
    LevelUID,
    levelUID,
  ) where


import System.FilePath

import Utils


-- Versioned constructors (for HighScore serialisation)
data LevelFile
    = StandardLevel_0 {levelDirectory :: FilePath, levelFilePath :: FilePath}
    | UserLevel_0 {levelDirectory :: FilePath, levelFilePath :: FilePath}
    | TemplateLevel_0 {levelFilePath :: FilePath}
    | UnknownLevelType_0 {levelFilePath :: FilePath}
  deriving (Show)

standardLevel = StandardLevel_0
userLevel = UserLevel_0
templateLevel = TemplateLevel_0
unknownLevel = UnknownLevelType_0


isUserLevel :: LevelFile -> Bool
isUserLevel UserLevel_0{} = True
isUserLevel _ = False

isTemplateLevel :: LevelFile -> Bool
isTemplateLevel TemplateLevel_0{} = True
isTemplateLevel _ = False

-- | short name of a level
levelName :: LevelFile -> String
levelName = takeBaseName . levelFilePath

type LevelUID = String

-- | unique  ID of a level
levelUID :: LevelFile -> LevelUID
levelUID (StandardLevel_0 levelDir levelPath) =
    "standardLevels" </> dropPrefix levelDir levelPath
levelUID (UserLevel_0 levelDir levelPath) =
    "userLevels" </> dropPrefix levelDir levelPath
levelUID (TemplateLevel_0 path) = path
levelUID (UnknownLevelType_0 path) = path
