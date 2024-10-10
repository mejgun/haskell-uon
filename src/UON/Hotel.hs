{-# LANGUAGE RecordWildCards #-}

module UON.Hotel (Hotel (..)) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)
import UON.Internal.Utils (textWithHtmlToText)
import UON.Types (Id)

data Hotel = Hotel
  { _id :: Id Hotel,
    name :: Text,
    name_en :: Text,
    contacts :: Maybe Text,
    country_id :: Int,
    country :: Maybe Text,
    city_id :: Int,
    city :: Maybe Text
  }
  deriving (Show)

instance FromJSON Hotel where
  parseJSON = withObject "Hotel" $ \obj -> do
    _id <- obj .: "id"
    name <- textWithHtmlToText <$> obj .: "name"
    name_en <- textWithHtmlToText <$> obj .: "name_en"
    contacts <- obj .: "contacts"
    country_id <- obj .: "country_id"
    country <- obj .: "country"
    city_id <- obj .: "city_id"
    city <- obj .: "city"
    pure Hotel {..}
