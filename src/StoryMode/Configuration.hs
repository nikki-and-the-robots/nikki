module StoryMode.Configuration where

import StoryMode.Types

episodes :: [Episode FilePath]
episodes =
    episode_1 :
    []

episode_1 = Episode
    Episode_1
    "00-intro"
       (
        "01-jetlab" :
        "02-platform-training" :
        "03-powergrid" :
        "04-hanoi" :
        "05-lasertower" :
        "06-suivez-moi" :
        "07-lootninja" :
        "08-superfly" :
        "09-tree-climbing" :
        "10-greed" :
        [])
    "level-x"
    "happy-end"

epPathSnippet Episode_1 = "ep1"

epTitle Episode_1 = "Episode 1"

batteryNumberNeeded :: Integer
batteryNumberNeeded = 250
