module UON.Lead.Update (update, UpdateParam (..)) where

import Data.Text qualified as T
import Network.HTTP.Req ((=:))
import UON qualified (Key)
import UON.Internal.Param (Param (toQuery))
import UON.Internal.Request qualified as Request
import UON.Lead (Lead (..))
import UON.Types (Id (..))

update :: UON.Key -> Id Lead -> [UpdateParam] -> IO (Either T.Text ())
update k (Id leadId) p = do
  let i = T.pack $ show leadId
  a <- Request.post ["request", "update", i] k p
  pure $ case a of
    Request.Status _ t -> Left t
    Request.Error t -> Left t
    Request.Ok r -> Right r

data UpdateParam
  = OfficeId Int
  | ManagerId Int
  | LeadStatusId Int
  | SourceId Int

instance Param UpdateParam where
  toQuery (OfficeId x) = "office_id" =: x
  toQuery (ManagerId x) = "manager_id" =: x
  toQuery (LeadStatusId x) = "lead_status_id" =: x
  toQuery (SourceId x) = "source_id" =: x
