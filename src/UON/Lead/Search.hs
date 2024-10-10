module UON.Lead.Search (search, SearchParam (..)) where

import Data.Aeson (FromJSON)
import Data.Text qualified as T
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Network.HTTP.Req ((=:))
import UON qualified (Key)
import UON.Internal.Param (Param (toQuery))
import UON.Internal.Request qualified as Request
import UON.Internal.Utils qualified as Utils
import UON.Lead (Lead)

newtype R = R {leads :: [Lead]} deriving (Generic)

instance FromJSON R

search :: UON.Key -> [SearchParam] -> IO (Either T.Text [Lead])
search k p = do
  a <- Request.post ["lead", "search"] k p
  pure $ case a of
    Request.Status 404 _ -> Right []
    Request.Status _ t -> Left t
    Request.Error t -> Left t
    Request.Ok R {leads = r} -> Right r

data SearchParam
  = OfficeIds [Int]
  | ManagerIds [Int]
  | StatusIds [Int]
  | SourceIds [Int]
  | DateCreateFrom LocalTime
  | DateCreateTo LocalTime
  | Page Int

instance Param SearchParam where
  toQuery (OfficeIds xs) = "office_ids" =: Utils.intListToString xs
  toQuery (ManagerIds xs) = "manager_ids" =: Utils.intListToString xs
  toQuery (StatusIds xs) = "status_ids" =: Utils.intListToString xs
  toQuery (SourceIds xs) = "source_ids" =: Utils.intListToString xs
  toQuery (DateCreateFrom d) = "date_create_from" =: Utils.utcToUonTime d
  toQuery (DateCreateTo d) = "date_create_to" =: Utils.utcToUonTime d
  toQuery (Page i) = "page" =: i
