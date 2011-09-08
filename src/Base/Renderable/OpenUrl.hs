
module Base.Renderable.OpenUrl where


import Graphics.Qt

import Utils

import Base.Types
import Base.Prose

import Base.Renderable.Message


openUrl :: Application -> String -> Parent -> AppState
openUrl app url parent = NoGUIAppState $ do
    success <- io $ qtOpenUrl url
    return $ if success then parent else message app text parent
  where
    text =
        p "Something went wrong with opening your webbrowser." :
        p "Please manually point your browser to:" :
        pv url :
        []
