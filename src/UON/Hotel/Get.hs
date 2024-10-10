module UON.Hotel.Get (get) where

import Data.Aeson (FromJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import UON qualified
import UON.Hotel (Hotel)
import UON.Internal.Request qualified as Request

newtype R = R {record :: [Hotel]} deriving (Generic)

instance FromJSON R

get :: UON.Key -> Int -> IO (Either T.Text Hotel)
get key hotel_id = do
  a <- Request.get ["hotel", T.pack (show hotel_id)] key mempty
  pure $ case a of
    Request.Ok R {record = []} -> Left "empty result"
    Request.Ok R {record = [r]} -> Right r
    Request.Ok R {record = _} -> Left "too many results"
    Request.Status _ t -> Left t
    Request.Error t -> Left t