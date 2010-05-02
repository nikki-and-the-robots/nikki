
module Game.Objects.Nikki.Types where


import Game.Animation

import Editor.Sprited


data State = State {
    direction :: FrameSetDirection,
    animation :: Animation
  }
    deriving Show


-- * constructors

initialState :: State
initialState = State ToRight UninitializedAnimation


-- * setter

setAnimation :: State -> Animation -> State
setAnimation s x = s{animation = x}

