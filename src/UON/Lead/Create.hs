module UON.Lead.Create (create, CreateParam (..)) where

import Data.Aeson (FromJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Req (FormUrlEncodedParam, (=:))
import UON qualified
import UON.Internal.Param (Param (toQuery))
import UON.Internal.Request qualified as Request
import UON.Lead (Lead)
import UON.Types (Id)

data CreateParam
  = Phone T.Text
  | Name T.Text
  | Note T.Text
  | OfficeId Int
  | ManagerId Int
  | SourceId Int
  | Source T.Text
  | ExtendedFields [(Int, T.Text)]

instance Param CreateParam where
  toQuery (Phone x) = "u_phone" =: x
  toQuery (Name x) = "u_name" =: x
  toQuery (Note x) = "note" =: x
  toQuery (OfficeId x) = "r_co_id" =: x
  toQuery (ManagerId x) = "r_u_id" =: x
  toQuery (SourceId x) = "source_id" =: x
  toQuery (Source x) = "source" =: x
  toQuery (ExtendedFields m) = p
    where
      -- тут генерируется массив
      -- как по ссылке, в примере №3
      -- https://www.php.net/manual/ru/function.http-build-query.php
      p :: FormUrlEncodedParam
      p =
        let f2 i = T.concat ["extended_fields[", T.pack (show i), "]"]
            f3 (k, v) = f2 k =: v
         in foldr ((<>) . f3) mempty m

data Res = Res
  { id :: Maybe (Id Lead),
    result :: Maybe Int
  }
  deriving (Generic)

instance FromJSON Res

create :: UON.Key -> [CreateParam] -> IO (Either T.Text (Maybe (Id Lead)))
create k p = do
  a <- Request.post ["lead", "create"] k p
  pure $ case a of
    Request.Ok (Res {id = i, result = Just 200}) -> Right i
    Request.Ok _ -> Right Nothing
    Request.Status _ t -> Left t
    Request.Error t -> Left t