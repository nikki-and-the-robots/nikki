
module Base.Renderable.OpenUrl where


import Graphics.Qt

import Utils

import Base.Types
import Base.Prose

import Base.Renderable.Message


openUrl :: Application -> String -> Parent -> AppState
openUrl app url parent = NoGUIAppState $ do
    success <- io $ qtOpenUrl url
    return $ message app (text success) parent
  where
    text False =
        p "Something went wrong with opening your webbrowser." :
        p "Please manually point your browser to:" :
        pv url :
        []
    text True =
        p "A browser should open up." :
        p "If not, please manually point your browser to:" :
        pv url :
        []
