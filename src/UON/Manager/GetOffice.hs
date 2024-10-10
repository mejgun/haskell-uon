module UON.Manager.GetOffice (getOffice, getCompany) where

import Data.Aeson (FromJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import UON.Internal.Request qualified as Request
import UON.Manager (Manager)
import UON.Office (Office)
import UON.Types (Id (..))

newtype R = R {users :: [Manager]} deriving (Generic)

instance FromJSON R

getOffice :: Request.Key -> Maybe (Id Office) -> IO (Either T.Text [Manager])
getOffice key office = do
  a <- Request.get ["manager", "office", T.pack (show (o office))] key mempty
  pure $ case a of
    Request.Ok R {users = r} -> Right r
    Request.Status 404 _ -> Right []
    Request.Status _ t -> Left t
    Request.Error e -> Left e
  where
    o (Just ((Id x))) = x
    o Nothing = 0

getCompany :: Request.Key -> IO (Either T.Text [Manager])
getCompany key = do
  a <- Request.get ["manager"] key mempty
  pure $ case a of
    Request.Ok R {users = r} -> Right r
    Request.Status _ t -> Left t
    Request.Error e -> Left e
