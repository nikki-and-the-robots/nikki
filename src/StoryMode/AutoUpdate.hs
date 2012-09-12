
module StoryMode.AutoUpdate where


import Control.Monad.Trans.Error

import Utils

import Base

import StoryMode.Client
import StoryMode.Purchasing


-- | auto updating of the storymode
update :: Configuration -> Application -> (Prose -> IO ()) -> ErrorT String IO ()
update config app logCommand = catchSomeExceptionsErrorT show $ do
    loginData <- readLoginData
    answer <- (convertErrorT (\ err -> "SERVER-ERROR:\n" ++ err) $
                askForStoryModeZip (story_mode_server_portnumber config) loginData)
    case answer of
        (Unauthorized err) ->
            throwError ("UNAUTHORIZED REQUEST:\n" ++ err)
        (AuthorizedDownload zipUrl version) ->
            installStoryMode app logCommand loginData version zipUrl
