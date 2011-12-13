
module StoryMode.Configuration where


import StoryMode.Types


purchasingUrl :: String
purchasingUrl = "http://joyridelabs.de/game/download"

episodes :: [Episode FilePath]
episodes =
    episode_1 :
    []

episode_1 = Episode
    Episode_1
    "00-intro"
       (
        "00-intro" :
        "01-grotto" :
        "02-platform-training" :
        "03-powergrid" :
        "04-hanoi" :
        "05-lasertower" :
        "06-suivez-moi" :
        "07-lootninja" :
        "09-tree-climbing" :
        "10-greed" :
        [])
    "x"

epPathSnippet Episode_1 = "ep1"

epTitle Episode_1 = "Episode 1"

