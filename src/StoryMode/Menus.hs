
module StoryMode.Menus where


import Data.Maybe
import Data.Map (Map, member)

import Utils

import Base

import Editor.Pickle.LevelFile

import StoryMode.Types
import StoryMode.Configuration
import StoryMode.Episode
import StoryMode.Purchasing


-- | shows a text describing our plans with the story mode
storyMode :: Application -> Play -> Parent -> AppState
storyMode app play parent = NoGUIAppState $ do
    mEpisodes <- io $ loadEpisodes
    case mEpisodes of
        Nothing -> return $ suggestPurchase app this parent 0
        Just episodes -> return $ mkEpisodesMenu app play parent episodes 0
  where
    this :: AppState
    this = storyMode app play parent

mkEpisodesMenu :: Application -> Play -> Parent -> [Episode LevelFile] -> Int -> AppState
mkEpisodesMenu app play parent episodes =
    menuAppState app
        (NormalMenu (p "story mode") (Just $ p "choose an episode"))
        (Just parent)
        (map (mkMenuItem app play this) episodes)
  where
    this = mkEpisodesMenu app play parent episodes

mkMenuItem :: Application -> Play -> (Int -> Parent)
    -> Episode LevelFile -> (Prose, Int -> AppState)
mkMenuItem app play parent e =
    (pv $ epTitle $ euid e, \ i -> mkEpisodeMenu app play (parent i) e 0)

mkEpisodeMenu :: Application -> Play -> Parent
    -> Episode LevelFile -> Int -> AppState
mkEpisodeMenu app play parent ep ps = NoGUIAppState $ do
    scores <- io $ getHighScores
    let passedIntro = hasPassedIntro scores ep
    introItem <- mkItem (intro ep)
    restItems <- if not passedIntro then return [] else do
        bodyItems <- mapM mkItem (body ep)
        outroItem <- mkItem (outro ep)
        return (bodyItems +: outroItem)
    return $ menuAppState app
        (NormalMenu (p "story mode") (Just $ p "choose a level"))
        (Just parent)
        (introItem :
        restItems ++
        [])
        ps
  where
    mkItem level = io $ do
        label <- showLevelForMenu level
        return (label, \ i -> play (this i) level)
    this = mkEpisodeMenu app play parent ep

hasPassedIntro :: Map LevelUID Score -> Episode LevelFile -> Bool
hasPassedIntro scores e = member (levelUID $ intro e) scores
