module UON.Request.Search (search, SearchParam (..)) where

import Data.Aeson (FromJSON)
import Data.Text qualified as T
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Network.HTTP.Req ((=:))
import UON qualified (Key)
import UON.Internal.Param (Param (toQuery))
import UON.Internal.Request qualified as Request
import UON.Internal.Utils qualified as Utils
import UON.Request

newtype R = R {requests :: [Request]} deriving (Generic)

instance FromJSON R

data SearchParam
  = IdSystem Int
  | OfficeIds [Int]
  | ClientIds [Int]
  | ManagerIds [Int]
  | DateLeadCreateFrom LocalTime
  | DateLeadCreateTo LocalTime
  | DateCreateFrom LocalTime
  | DateCreateTo LocalTime
  | DateBeginFrom LocalTime
  | DateBeginTo LocalTime
  | DateEndFrom LocalTime
  | DateEndTo LocalTime
  | StatusIds [Int]
  | SourceIds [Int]
  | ServiceTypeIds [Int]
  | PayStatusIds [Int]
  | Page Int
  | CalcClientFrom Double -- Оплаты клиента (от)
  | ExtField Int T.Text

instance Param SearchParam where
  toQuery (IdSystem x) = "r_id_system" =: x
  toQuery (OfficeIds xs) = "office_ids" =: Utils.intListToString xs
  toQuery (ClientIds xs) = "client_ids" =: Utils.intListToString xs
  toQuery (ManagerIds xs) = "manager_ids" =: Utils.intListToString xs
  toQuery (DateLeadCreateFrom d) = "date_lead_create_from" =: Utils.utcToUonTime d
  toQuery (DateLeadCreateTo d) = "date_lead_create_to" =: Utils.utcToUonTime d
  toQuery (DateCreateFrom d) = "date_create_from" =: Utils.utcToUonTime d
  toQuery (DateCreateTo d) = "date_create_to" =: Utils.utcToUonTime d
  toQuery (DateBeginFrom d) = "date_begin_from" =: Utils.utcToUonTime d
  toQuery (DateBeginTo d) = "date_begin_to" =: Utils.utcToUonTime d
  toQuery (DateEndFrom d) = "date_end_from" =: Utils.utcToUonTime d
  toQuery (DateEndTo d) = "date_end_to" =: Utils.utcToUonTime d
  toQuery (StatusIds xs) = "status_ids" =: Utils.intListToString xs
  toQuery (SourceIds xs) = "source_ids" =: Utils.intListToString xs
  toQuery (ServiceTypeIds xs) = "service_type_ids" =: Utils.intListToString xs
  toQuery (PayStatusIds xs) = "pay_status_ids" =: Utils.intListToString xs
  toQuery (CalcClientFrom x) = "calc_client_from" =: x
  toQuery (Page i) = "page" =: i
  toQuery (ExtField k v) = ("ext_fields[" <> T.pack (show k) <> "]") =: v

search :: UON.Key -> [SearchParam] -> IO (Either T.Text [Request])
search k p = do
  a <- Request.post ["request", "search"] k p
  pure $ case a of
    Request.Status 404 _ -> Right []
    Request.Status _ t -> Left t
    Request.Error t -> Left t
    Request.Ok R {requests = r} -> Right r