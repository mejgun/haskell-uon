module UON.Reminder.Create
  ( create,
    CreateParam (..),
    RType (..),
  )
where

import Data.Text qualified as T
import Data.Time (LocalTime)
import Network.HTTP.Req ((=:))
import UON qualified
import UON.Internal.Param (Param (toQuery))
import UON.Internal.Request qualified as Request
import UON.Internal.Utils qualified as Utils
import UON.Manager (Manager)
import UON.Request (Request)
import UON.Types (Id (..))

create :: UON.Key -> [CreateParam] -> IO (Either T.Text ())
create k p = do
  a <- Request.post ["reminder", "create"] k p
  pure $ case a of
    Request.Status _ t -> Left t
    Request.Error t -> Left t
    Request.Ok r -> Right r

data CreateParam
  = RequestId (Id Request)
  | ManagerId (Id Manager)
  | Type RType
  | Text T.Text
  | DateTime LocalTime

data RType
  = Undefined
  | Call
  | Mail
  | Meeting

toInt :: RType -> Int
toInt Undefined = 0
toInt Call = 1
toInt Mail = 2
toInt Meeting = 3

instance Param CreateParam where
  toQuery (RequestId x) = "r_id" =: x.unId
  toQuery (ManagerId x) = "manager_id" =: x.unId
  toQuery (Type x) = "type_id" =: toInt x
  toQuery (Text x) = "text" =: x
  toQuery (DateTime d) = "datetime" =: Utils.utcToUonTime d
