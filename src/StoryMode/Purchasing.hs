
module StoryMode.Purchasing where


import Graphics.Qt

import Utils

import Base

import StoryMode.Configuration


suggestPurchase :: Application -> Parent -> Int -> AppState
suggestPurchase app parent =
    menuAppState app
        (NormalMenu (p "story mode") (Just $ p "the story mode is not installed"))
        (Just parent)
        ((p "buy the story mode", buyStoryMode app . this) :
         (p "login", login app . this) :
         [])
  where
    this = suggestPurchase app parent

buyStoryMode :: Application -> Parent -> AppState
buyStoryMode app parent = NoGUIAppState $ do
    success <- io $ qtOpenUrl purchasingURL
    return $ if success then parent else urlOpeningError app parent

urlOpeningError :: Application -> Parent -> AppState
urlOpeningError app parent = scrollingAppState app text parent
  where
    text =
        p "Something went wrong with opening your webbrowser. Please manually point your browser to $URL." :
        []
--         TODO: templating

login app f = message app [pv "NYI: login"] f
