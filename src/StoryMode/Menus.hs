
module StoryMode.Menus where


import Data.SelectTree
import Data.Maybe

import Utils

import Base

import Editor.Pickle.LevelFile

import StoryMode.Types
import StoryMode.Configuration
import StoryMode.Episode


-- | shows a text describing our plans with the story mode
storyMode :: Application -> (Parent -> LevelFile -> AppState) -> Parent -> AppState
storyMode app play parent = NoGUIAppState $ do
    mEpisodes <- io $ loadEpisodes
    case mEpisodes of
        Nothing -> do
            file <- rm2m $ getDataFileName "manual/storyModeNotBought"
            prose <- io $ pFile file
            return $ scrollingAppState app prose parent
        Just episodes -> do
            let epTree = mkEpisodesTree episodes
            return $ treeToMenu app parent (p "choose a level") showLevelForMenu epTree
                play 0

mkEpisodesTree :: [Episode LevelFile] -> SelectTree LevelFile
mkEpisodesTree = mkNode "story mode" . fmap mkEpisodeTree

mkEpisodeTree :: Episode LevelFile -> SelectTree LevelFile
mkEpisodeTree e =
    mkNode (epTitle $ euid e) (
        Leaf "intro" (intro e) :
        bodyNode :
        Leaf "outro" (outro e) :
        [])
  where
    bodyNode = mkNode "body levels" (map mkLevelLeaf $ body e)
    mkLevelLeaf l = Leaf (fromMaybe "???" (levelName l)) l
