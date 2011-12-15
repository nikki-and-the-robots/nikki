
module StoryMode.AutoUpdate where


import Control.Monad.Trans.Error

import Base


-- | auto updating of the storymode
update :: Application -> (Prose -> IO ()) -> ErrorT [String] IO ()
update app logCommand = throwError ["NYI: update"]
