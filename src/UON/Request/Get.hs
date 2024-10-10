module UON.Request.Get (get) where

import Data.Aeson (FromJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import UON qualified (Key)
import UON.Internal.Request qualified as Request
import UON.Request (Request)
import UON.Types (Id (..))

newtype R = R {request :: [Request]} deriving (Generic)

instance FromJSON R

get :: UON.Key -> Id Request -> IO (Either T.Text [Request])
get k (Id rId) = do
  a <- Request.get ["request", T.pack (show rId)] k mempty
  pure $ case a of
    Request.Status _ t -> Left t
    Request.Error t -> Left t
    Request.Ok R {request = r} -> Right r