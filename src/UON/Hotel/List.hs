module UON.Hotel.List (list) where

import Data.Aeson (FromJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import UON qualified
import UON.Hotel (Hotel)
import UON.Internal.Request qualified as Request

newtype R = R {records :: [Hotel]} deriving (Generic)

instance FromJSON R

list :: UON.Key -> Int -> IO (Either T.Text [Hotel])
list key page = do
  a <- Request.get ["hotels", T.pack (show page)] key mempty
  pure $ case a of
    Request.Ok R {records = r} -> Right r
    Request.Status 404 _ -> Right []
    Request.Status _ t -> Left t
    Request.Error t -> Left t