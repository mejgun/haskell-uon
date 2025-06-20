module UON.Payment
  ( create,
    SearchParam (..),
    TypeId (..),
    CioId (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Req ((=:))
import UON qualified (Key)
import UON.Internal.Param (Param (toQuery))
import UON.Internal.Request qualified as Request

data SearchParam
  = Id Int -- ID заявки
  | CioId CioId -- Вид платежа
  | TypeId TypeId -- Тип платежа
  | Price Double -- Стоимость клиенту
  | FormId Int -- ID вида платежа
  | Note T.Text -- Примечание по платежу
  | Reason T.Text -- Основание платежа

data TypeId = Clients | Partners

data CioId = Arrival | Expense

instance Param SearchParam where
  toQuery (Id x) = "r_id" =: x
  toQuery (Price x) = "price" =: x
  toQuery (Note x) = "note" =: x
  toQuery (Reason x) = "reason" =: x
  toQuery (FormId x) = "form_id" =: x
  toQuery (TypeId x) =
    "type_id" =: case x of
      Clients -> 1 :: Int
      Partners -> 2
  toQuery (CioId x) =
    "cio_id" =: case x of
      Arrival -> 1 :: Int
      Expense -> 2

newtype Res = Res {result :: Maybe Int}
  deriving (Generic)

instance FromJSON Res

create :: UON.Key -> [SearchParam] -> IO (Either T.Text ())
create k p = do
  a <- Request.post ["payment", "create"] k p
  case a of
    Request.Ok (v :: Aeson.Value) ->
      case Aeson.fromJSON v of
        (Aeson.Success (Res {result = Just 200})) -> pure $ Right ()
        _ -> pure $ Left $ "unknown result: " <> T.pack (show v)
    Request.Status _ t -> pure $ Left t
    Request.Error t -> pure $ Left t
