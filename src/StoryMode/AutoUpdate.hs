
module StoryMode.AutoUpdate where


import Control.Monad.Trans.Error

import Utils

import Base


-- | auto updating of the storymode
update :: Application -> ErrorT [String] IO ()
update _ = throwError ["NYI: update"]
