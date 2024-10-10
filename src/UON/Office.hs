module UON.Office (list, Office (..)) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import UON qualified (Key)
import UON.Internal.Request qualified as Request
import UON.Internal.Utils (textWithHtmlToText)
import UON.Types (Id (..))

newtype R = R {records :: [Office]} deriving (Generic)

instance FromJSON R

data Office = Office
  { _id :: Id Office,
    name :: Text,
    address :: Text
  }
  deriving (Show)

instance FromJSON Office where
  parseJSON = withObject "Office" $ \obj -> do
    _id <- obj .: "id"
    name <- obj .: "name"
    address <- obj .: "address"
    pure $
      Office
        { _id = _id,
          name = textWithHtmlToText name,
          address = address
        }

list :: UON.Key -> IO (Either T.Text [Office])
list key = do
  a <- Request.get ["company-office"] key mempty
  pure $ case a of
    Request.Ok R {records = r} -> Right r
    Request.Status 404 _ -> Right []
    Request.Status _ t -> Left t
    Request.Error t -> Left t