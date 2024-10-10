module UON.Lead (Lead (..)) where

-- лиды

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.HashMap.Strict qualified as HM
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Time qualified
import UON.Internal.Utils qualified as Utils
import UON.Manager (Manager)
import UON.Types (Id (..))

data Lead = Lead
  { _id :: Id Lead,
    office_id :: Int,
    source :: Maybe T.Text,
    source_id :: Maybe Int,
    notes :: Maybe T.Text,
    status_id :: Int,
    manager_id :: Maybe (Id Manager),
    name :: Maybe T.Text,
    phones :: [T.Text],
    dat_updated :: Maybe Data.Time.LocalTime,
    extended_fields :: HM.HashMap Int T.Text
  }
  deriving (Show)

instance Eq Lead where
  a == b = a._id == b._id

instance FromJSON Lead where
  parseJSON = withObject "lead" $ \obj -> do
    _id <- obj .: "id"
    office_id <- obj .: "office_id"
    source <- Utils.nonEmptyText obj "source"
    source_id <- obj .: "source_id"
    status_id <- obj .: "status_id"
    manager_id <- obj .: "manager_id"
    notes <- obj .: "notes"
    name <- Utils.nonEmptyText obj "client_name"
    phone <- obj .: "client_phone"
    phoneHome <- obj .: "client_phone_home"
    phoneMobile <- obj .: "client_phone_mobile"
    dat_updated <- Utils.readDateTime obj "dat_updated"
    extf_list <- obj .: "extended_fields"
    extf <- mapM (\x -> (,) <$> x .: "id" <*> x .: "value") extf_list
    pure $
      Lead
        { _id = _id,
          office_id = office_id,
          source = source,
          source_id = source_id,
          status_id = read status_id,
          notes = notes,
          manager_id = manager_id,
          name = name,
          phones = filter (not . T.null) $ catMaybes [phone, phoneHome, phoneMobile],
          dat_updated = dat_updated,
          extended_fields = HM.fromList (filter (not . T.null . snd) extf)
        }
