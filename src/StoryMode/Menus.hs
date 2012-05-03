
module StoryMode.Menus (
    newStoryModeAvailability,
    storyModeMenuItem,
    storyMode,
  ) where


import Data.Map (Map, member)
import Data.Maybe

import Control.Concurrent
import Control.Concurrent.MVar

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

newStoryModeAvailability :: Ptr MainWindow -> IO (MVar StoryModeAvailability)
newStoryModeAvailability window = do
    ref <- newEmptyMVar
    forkIO $ do
        lookForStoryModeSite >>= putMVar ref
        updateMainWindow window
    return ref

lookForStoryModeSite :: IO StoryModeAvailability
lookForStoryModeSite = do
    isInstalled <- isJust <$> loadEpisodes
    if isInstalled then
        return Installed
      else
        either (const NotAvailable) (const Buyable) <$>
        downloadLazy purchasingUrl

storyModeMenuItem :: StoryModeMenuItem
storyModeMenuItem = StoryModeMenuItem False

data StoryModeMenuItem = StoryModeMenuItem {selected :: Bool}

instance Renderable StoryModeMenuItem where
    render ptr app config size (StoryModeMenuItem selected) = do
        available <- tryReadMVar $ storyModeAvailability app
        let prose = case available of
                Nothing -> selMod $ p "story mode"
                Just Installed -> selMod $ p "story mode"
                Just NotAvailable -> selMod $ p "story mode (coming soon!)"
                Just Buyable -> colorizeProse yellow $ selMod $
                    p "story mode (buy now!)"
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
