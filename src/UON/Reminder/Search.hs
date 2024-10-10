module UON.Reminder.Search (search, SearchParam (..)) where

import Data.Text qualified as T
import Data.Time (LocalTime)
import Network.HTTP.Req ((=:))
import UON qualified
import UON.Internal.Param (Param (..))
import UON.Internal.Request qualified as Request
import UON.Internal.Utils qualified as Utils
import UON.Manager (Manager)
import UON.Reminder (Reminder)
import UON.Types (Id (..))

search :: UON.Key -> Int -> [SearchParam] -> IO (Either T.Text [Reminder])
search k page p = do
  a <- Request.post ["reminder", T.pack (show page)] k p
  pure $ case a of
    Request.Status _ e -> Left e
    Request.Error t -> Left t
    Request.Ok r -> Right r

data SearchParam
  = ManagerId (Id Manager)
  | DateFrom LocalTime
  | DateTo LocalTime
  | CreatedDateFrom LocalTime
  | CreatedDateTo LocalTime

instance Param SearchParam where
  toQuery (ManagerId xs) = "user_id" =: xs.unId
  toQuery (DateFrom d) = "date_from" =: Utils.utcToUonTime d
  toQuery (DateTo d) = "date_to" =: Utils.utcToUonTime d
  toQuery (CreatedDateFrom d) = "created_date_from" =: Utils.utcToUonTime d
  toQuery (CreatedDateTo d) = "created_date_to" =: Utils.utcToUonTime d
