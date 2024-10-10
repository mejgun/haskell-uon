module UON.Types (Id (..)) where

import Data.Aeson (FromJSON (parseJSON), Value (Number))
import Data.Scientific qualified as S

newtype Id a = Id {unId :: Int}
  deriving (Show, Eq)

instance FromJSON (Id a) where
  parseJSON (Number n) =
    case S.toBoundedInteger n of
      Just v -> pure $ Id v
      Nothing -> fail $ show n <> "cannot read a number"
  parseJSON x = fail $ show x <> "not a number"