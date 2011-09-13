{-# language DeriveDataTypeable #-}

module Base.Prose.Template (
    Base.Prose.Template.substitute
  ) where


import Data.Text
import qualified Data.Text.Lazy as TL
import Data.Text.Template as Template
import Data.Map
import Data.Maybe
import Data.Typeable

import Control.Arrow
import Control.Exception

import Base.Prose


substitute :: [(String, String)] -> Prose -> Prose
substitute context =
    mapText (TL.toStrict . flip Template.substitute (mkContext context))

mapText :: (Text -> Text) -> Prose -> Prose
mapText f (Prose a) = Prose $ fmap (second f) a

mkContext :: [(String, String)] -> Context
mkContext context =
    let m :: Map Text Text
        m = fromList $ fmap (first pack . second pack) context
    in \ key -> fromMaybe
            (throw $ TemplateIdentifierMissing $ unpack key)
            (Data.Map.lookup key m)

data TemplateIdentifierMissing = TemplateIdentifierMissing String
  deriving (Show, Typeable)

instance Exception TemplateIdentifierMissing
