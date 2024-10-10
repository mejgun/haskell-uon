module UON.Hotel.Update (update, UpdateParam (..)) where

import Data.Text qualified as T
import Network.HTTP.Req ((=:))
import UON qualified
import UON.Hotel (Hotel)
import UON.Internal.Param (Param (toQuery))
import UON.Internal.Request qualified as Request
import UON.Types (Id (Id))

update :: UON.Key -> Id Hotel -> [UpdateParam] -> IO (Either T.Text ())
update k (Id hotelId) p = do
  let i = T.pack $ show hotelId
  a <- Request.post ["hotel", "update", i] k p
  pure $ case a of
    Request.Status _ t -> Left t
    Request.Error t -> Left t
    Request.Ok r -> Right r

data UpdateParam
  = Contacts T.Text
  | Notice T.Text

instance Param UpdateParam where
  toQuery (Contacts x) = "contacts" =: x
  toQuery (Notice x) = "notice" =: x