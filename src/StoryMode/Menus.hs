
module StoryMode.Menus (
    newStoryModeAvailability,
    storyModeMenuItem,
    storyMode,
  ) where


import Data.Map (lookup, findWithDefault)
import Data.Maybe
import Data.Initial

import Control.Concurrent

import System.FilePath

import Network.Download

import Graphics.Qt

import Utils

import Base

import Editor.Pickle.LevelFile

import StoryMode.Types
import StoryMode.Configuration
import StoryMode.Episode
import StoryMode.Purchasing


-- * item in main menu

newStoryModeAvailability :: Ptr MainWindow -> Configuration -> IO (MVar StoryModeAvailability)
newStoryModeAvailability window config = do
    ref <- newEmptyMVar
    _ <- forkIO $ do
        lookForStoryModeSite config >>= putMVar ref
        updateMainWindow window
    return ref

lookForStoryModeSite :: Configuration -> IO StoryModeAvailability
lookForStoryModeSite config = do
    isInstalled <- isJust <$> loadEpisodes
    if isInstalled then
        return Installed
      else
        either (const NotAvailable) (const Buyable) <$>
        downloadLazy (fromMaybe defaultPurchasingUrl (story_mode_purchasing_url config))

storyModeMenuItem :: StoryModeMenuItem
storyModeMenuItem = StoryModeMenuItem False

data StoryModeMenuItem = StoryModeMenuItem {selected :: Bool}

instance Renderable StoryModeMenuItem where
    render ptr app config size (StoryModeMenuItem selected) = do
        available <- tryReadMVar $ storyModeAvailability app
        let prose = case available of
                Nothing -> selMod $ p "Story Episodes"
                Just Installed -> selMod $ p "Story Episodes"
                Just NotAvailable -> selMod $ p "Story Episodes (coming soon!)"
                Just Buyable -> colorizeProse yellow $ selMod $
                    p "Story Episodes (buy now!)"
            selMod = if selected then select else deselect
        render ptr app config size prose

    label = const "StoryModeMenuItem"
    select = const $ StoryModeMenuItem True
    deselect = const $ StoryModeMenuItem False


-- * story mode menu itself

-- | Menu for the story mode
storyMode :: Application -> Play -> Parent -> AppState
storyMode app play parent = NoGUIAppState $ do
    mEpisodes <- io $ loadEpisodes
    case mEpisodes of
        Nothing -> return $ suggestPurchase app this parent 0
        Just episodes -> return $ mkEpisodesMenu app play parent episodes 0
  where
    this :: AppState
    this = storyMode app play parent

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
    mFile <- getStoryModeDataFileName ("manual" </> "credits" <.> "txt")
    prose <- maybe
                (return $ pure $ p "storymode not found")
                pFile
                mFile
    return $ scrollingAppState app prose parent
