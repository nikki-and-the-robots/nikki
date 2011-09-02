
module StoryMode.Configuration where


import StoryMode.Types


purchasingURL :: String
purchasingURL = "http://joyridelabs.de/game/download"

episodes :: [Episode FilePath]
episodes =
    episode_1 :
    []

episode_1 = Episode
    Episode_1
    "ep1-intro-v02"
    (
        "stash" :
        "stush" :
    [])
    "ep1-x-v03"

epPathSnippet Episode_1 = "ep1"

epTitle Episode_1 = "Episode 1"
