
module StoryMode.Purchasing where


import Utils

import Base

import StoryMode.Configuration


suggestPurchase :: Application -> Parent -> Int -> AppState
storyMode app parent = \ ps -> NoGUIAppState $ do
    file <- rm2m $ getDataFileName "manual/storyModeIntroduction"
    prose <- io $ pFile file
    return $ scrollingAppState app prose parent
suggestPurchase app parent =
    menuAppState app
        (NormalMenu (p "story mode") (Just $ p "the story mode is not installed"))
        (Just parent)
        ((p "buy the story mode", openUrl app purchasingUrl . this) :
         (p "login", login app . this) :
         [])
  where
    this = suggestPurchase app parent

login app f = message app [pv "NYI: login"] f
