module UON.Request.Update (update, UpdateParam (..)) where

import Data.Text qualified as T
import Network.HTTP.Req ((=:))
import UON qualified (Key)
import UON.Internal.Param (Param (toQuery))
import UON.Internal.Request qualified as Request
import UON.Request (Request)
import UON.Types (Id (..))

data UpdateParam
  = OfficeId Int
  | ManagerId Int
  | ExtField Int T.Text
  | SourceId Int

instance Param UpdateParam where
  toQuery (OfficeId x) = "office_id" =: x
  toQuery (ManagerId x) = "manager_id" =: x
  toQuery (ExtField k v) = ("extended_fields[" <> T.pack (show k) <> "]") =: v
  toQuery (SourceId x) = "source_id" =: x

update :: UON.Key -> Id Request -> [UpdateParam] -> IO (Either T.Text ())
update k (Id rId) p = do
  a <- Request.post ["request", "update", T.pack (show rId)] k p
  pure $ case a of
    Request.Status _ t -> Left t
    Request.Error t -> Left t
    Request.Ok () -> Right ()