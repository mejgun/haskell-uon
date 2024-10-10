{-# LANGUAGE RecordWildCards #-}

module UON.Reminder (Reminder (..)) where

import Data.Aeson (FromJSON (parseJSON), withObject)
import Data.Aeson.Types ((.:))
import Data.Text qualified as T
import UON.Manager (Manager)
import UON.Types (Id)

data Reminder = Reminder
  { _id :: Int,
    text :: T.Text,
    manager_id :: Id Manager
  }
  deriving (Show)

instance FromJSON Reminder where
  parseJSON = withObject "Reminder" $ \obj -> do
    _id <- obj .: "id"
    text <- obj .: "text"
    manager_id <- obj .: "manager_id"
    pure $ Reminder {..}
