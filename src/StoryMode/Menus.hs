module StoryMode.Menus (
    storyMode,
  ) where


import Data.Map (lookup, findWithDefault)
import Data.Initial

import System.FilePath

import Utils hiding (tryReadMVar)

import Base

import Editor.Pickle.LevelFile

import StoryMode.Types
import StoryMode.Configuration
import StoryMode.Episode

-- * story mode menu itself

-- | Menu for the story mode
storyMode :: Application -> Play -> Parent -> AppState
storyMode app play parent = NoGUIAppState $ io $ do
    episodes <- loadEpisodes
    return $ mkEpisodesMenu app play parent episodes 0

-- | a menu showing all available episodes
mkEpisodesMenu :: Application -> Play -> Parent -> [Episode LevelFile] -> Int -> AppState
mkEpisodesMenu app play parent episodes =
    menuAppState app
        (NormalMenu (p "Story Episodes") (Just $ p "choose an episode"))
        (Just parent)
        (map (mkMenuItem app play this) episodes)
  where
    this = mkEpisodesMenu app play parent episodes

mkMenuItem :: Application -> Play -> (Int -> Parent)
    -> Episode LevelFile -> MenuItem
mkMenuItem app play parent e =
    MenuItem (pv $ epTitle $ euid e) (\ i -> mkEpisodeMenu app play (parent i) e 0)

-- | a menu for one episode.
mkEpisodeMenu :: Application -> Play -> Parent
    -> Episode LevelFile -> Int -> AppState
mkEpisodeMenu app play parent ep ps = NoGUIAppState $ do
    scores <- io $ getScores
    let passedIntro = hasPassedIntro (highScores scores) ep
        introItem = mkItem scores False (intro ep)
        restItems = if not passedIntro then [] else
            let bodyItems = map (mkItem scores False) (body ep)
                outroItem = mkItem scores True (outro ep)
            in (bodyItems +: outroItem)
        happyEndItem = if episodeCompleted scores
            then pure $ mkItem scores False (happyEnd ep)
            else []
        creditsItem = MenuItem (renderable $ p "credits") (credits app . this)
    return $ menuAppState app
        (NormalMenu (p "Story Episodes") (Just $ p "choose a level"))
        (Just parent)
        (introItem :
         restItems ++
         happyEndItem ++
         creditsItem :
         [])
        ps
  where
    episodeCompleted :: HighScoreFile -> Bool
    episodeCompleted hsf =
        let outroScore = Data.Map.lookup (levelUID $ outro ep) (highScores hsf)
        in maybe False isPassedScore outroScore

    mkItem :: HighScoreFile -> Bool -> LevelFile -> MenuItem
    mkItem scores isOutro level = MenuItem
        (showLevel scores isOutro level)
        (\ i -> play (this i) level)
    showLevel :: HighScoreFile -> Bool -> LevelFile -> Prose
    showLevel scores isOutro = if isOutro
        then showOutroLevelForMenu (highScores scores) ep
                (findWithDefault initial (euid ep) (episodeScores scores))
        else showLevelForMenu (highScores scores)
    this = mkEpisodeMenu app play parent ep

hasPassedIntro :: Scores -> Episode LevelFile -> Bool
hasPassedIntro scores e =
    maybe False isPassedScore $
    Data.Map.lookup (levelUID $ intro e) scores

credits :: Application -> Parent -> AppState
credits app parent = NoGUIAppState $ io $ do
    file <- getStoryModeDataFileName ("manual" </> "credits" <.> "txt")
    prose <- pFile file
    return $ scrollingAppState app prose parent
