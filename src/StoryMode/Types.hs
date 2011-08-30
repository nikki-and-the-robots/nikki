
module StoryMode.Types where


import Data.Binary
import Data.Foldable
import Data.Traversable
import Data.Initial

import Utils


-- | Versioned!
-- Saves the state of an episode.
-- 1. How many batteries were put in the battery terminal.
-- (How many batteries are collected in total will be calculated by the
-- highscores.)
-- (Which stage is available (intro, body, outro) should be calculated by the highscores.)
data EpisodeScore
    = EpisodeScore_0 {
        usedBatteryTerminal :: Bool,
        batteriesInTerminal :: Integer
      }
  deriving Show

instance Initial EpisodeScore where
    initial = EpisodeScore_0 False 0

instance Binary EpisodeScore where
    put (EpisodeScore_0 ubt batts) = do
        putWord8 0
        put ubt
        put batts
    get = do
        0 <- getWord8
        EpisodeScore_0 <$> get <*> get

data EpisodeUID
    = Episode_1
  deriving (Show, Eq, Ord)

instance Binary EpisodeUID where
    put Episode_1 = putWord8 1
    get = do
        1 <- getWord8
        return Episode_1

data Episode a = Episode {
    euid :: EpisodeUID,
    intro :: a,
    body :: [a],
    outro :: a
  }
    deriving Show

instance Functor Episode where
    fmap f (Episode uid i b o) =
        Episode uid (f i) (fmap f b) (f o)

instance Foldable Episode where
    foldMap f (Episode _ i b o) =
        f i +> foldMap f b +> f o

instance Traversable Episode where
    traverse f (Episode uid intro body outro) =
        Episode uid <$> f intro <*> traverse f body <*> f outro
