{-# LANGUAGE RecordWildCards #-}

module UON.Request
  ( Request (..),
    Hotel (..),
    Tourist (..),
    TouristKind (..),
    TouristSex (..),
  )
where

-- заявfки

import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    Value (Number),
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Time qualified
import Data.Vector qualified as V
import UON.Internal.Utils qualified as Utils
import UON.Manager (Manager)
import UON.Types (Id (..))

data Request = Request
  { _id :: Id Request,
    idSystem :: Int, -- номер договора/заявки
    source :: Maybe T.Text,
    source_id :: Maybe Int,
    reservation_number :: Maybe T.Text,
    calc_price :: Maybe Double, -- стоимость тура для туриста
    calc_price_netto :: Maybe Double, -- себестоимость тура
    calc_client :: Maybe Double, -- оплачено туристом
    calc_partner :: Maybe Double, -- оплачено партнёрам
    client_surname :: Maybe T.Text, -- фамилия туриста
    client_name :: Maybe T.Text,
    client_id :: Maybe Int,
    dat_lead :: Maybe Data.Time.LocalTime, -- дата создания лида??
    created_at :: Maybe Data.Time.LocalTime, -- дата создания лида??
    dat :: Maybe Data.Time.LocalTime, -- дата создания
    dat_request :: Maybe Data.Time.LocalTime, -- дата создания реальная
    date_begin :: Maybe Data.Time.LocalTime, -- дата начала
    date_end :: Maybe Data.Time.LocalTime, -- дата окончания
    status_id :: Int,
    travel_type_id :: Maybe Int,
    manager_id :: Id Manager,
    manager_name :: Maybe T.Text,
    manager_surname :: Maybe T.Text,
    office_id :: Int,
    office_name :: Maybe T.Text,
    extended_fields :: HM.HashMap Int T.Text,
    phones :: [T.Text],
    tourists :: [Tourist],
    service_hotels :: [Hotel]
  }
  deriving (Show)

instance Eq Request where
  a == b = a._id == b._id

data Hotel = Hotel
  { hotel_id :: Maybe Int,
    hotel :: Maybe T.Text,
    hotel_en :: Maybe T.Text,
    date_begin :: Maybe Data.Time.LocalTime,
    date_end :: Maybe Data.Time.LocalTime,
    tourists :: [Tourist]
  }
  deriving (Show)

data Tourist = Tourist
  { kind :: TouristKind,
    sex :: TouristSex,
    surname :: Maybe T.Text,
    name :: Maybe T.Text,
    sname :: Maybe T.Text,
    surname_en :: Maybe T.Text,
    name_en :: Maybe T.Text,
    birthday :: Maybe Data.Time.LocalTime
  }
  deriving (Show)

instance FromJSON Tourist where
  parseJSON = withObject "tourist" parseTouristJSON

parseTouristJSON :: Object -> Parser Tourist
parseTouristJSON obj = do
  kind <- obj .: "tourist_kind"
  sex <- obj .: "u_sex"
  surname <- Utils.nonEmptyText obj "u_surname"
  name <- Utils.nonEmptyText obj "u_name"
  sname <- Utils.nonEmptyText obj "u_sname"
  surname_en <- Utils.nonEmptyText obj "u_surname_en"
  name_en <- Utils.nonEmptyText obj "u_name_en"
  birthday <- Utils.readDateTime obj "u_birthday"
  pure $ Tourist {..}

data TouristKind
  = UnknownKind
  | Mr
  | Mrs
  | Miss
  | Child
  | Infant
  deriving (Show)

instance FromJSON TouristKind where
  parseJSON (Number 1) = pure Mr
  parseJSON (Number 2) = pure Mrs
  parseJSON (Number 3) = pure Miss
  parseJSON (Number 4) = pure Child
  parseJSON (Number 5) = pure Infant
  parseJSON _ = pure UnknownKind

data TouristSex
  = UnknownSex
  | Male
  | Female
  deriving (Show)

instance FromJSON TouristSex where
  parseJSON (Number 1) = pure Male
  parseJSON (Number 2) = pure Female
  parseJSON _ = pure UnknownSex

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj -> do
    _id <- obj .: "id"
    idSystem <- obj .: "id_system"
    manager_id <- obj .: "manager_id"
    manager_name <- Utils.nonEmptyText obj "manager_name"
    manager_surname <- Utils.nonEmptyText obj "manager_surname"
    travel_type_id <- obj .: "travel_type_id"
    status_id <- read <$> obj .: "status_id"
    office_id <- obj .: "office_id"
    office_name <- Utils.nonEmptyText obj "office_name"
    source <- Utils.nonEmptyText obj "source"
    source_id <- obj .: "source_id"
    client_surname <- Utils.nonEmptyText obj "client_surname"
    client_name <- Utils.nonEmptyText obj "client_name"
    client_id <- obj .: "client_id"
    calc_price <- obj .:? "calc_price"
    calc_client <- obj .:? "calc_client"
    calc_partner <- obj .:? "calc_partner"
    calc_price_netto <- obj .: "calc_price_netto"
    dat <- Utils.readDateTime obj "dat"
    dat_lead <- Utils.readDateTime obj "dat_lead"
    created_at <- Utils.readDateTime obj "created_at"
    dat_request <- Utils.readDateTime obj "dat_request"
    date_begin <- Utils.readDateTime obj "date_begin"
    date_end <- Utils.readDateTime obj "date_end"
    extf_list <- obj .: "extended_fields"
    reservation_number <- Utils.nonEmptyText obj "reservation_number"
    services <- obj .: "services"
    vectour <- obj .: "tourists"
    tourists <- obj .: "tourists"
    service_hotels <- catMaybes . V.toList <$> parseServices services vectour
    phones <- do
      phone <- Utils.nonEmptyText obj "client_phone"
      phoneHome <- Utils.nonEmptyText obj "client_phone_home"
      phoneMobile <- Utils.nonEmptyText obj "client_phone_mobile"
      pure $ catMaybes [phone, phoneHome, phoneMobile]
    extended_fields <-
      HM.fromList
        <$> mapM
          ( \x -> do
              k <- x .: "id"
              v <- x .: "value"
              pure (k, v)
          )
          extf_list
    pure Request {..}

parseServices :: V.Vector Object -> V.Vector Object -> Parser (V.Vector (Maybe Hotel))
parseServices srvs trsts =
  mapM go srvs
  where
    go :: Object -> Parser (Maybe Hotel)
    go el = do
      t :: Int <- el .: "service_type_id"
      if t == 1
        then do
          hotel_id <- el .: "hotel_id"
          hotel <- Utils.nonEmptyText el "hotel"
          hotel_en <- Utils.nonEmptyText el "hotel_en"
          date_begin <- Utils.readDateTime el "date_begin"
          date_end <- Utils.readDateTime el "date_end"
          trsts_list <- el .: "tourists"
          trsts_ids <- nub <$> mapM (.: "tourist_id") trsts_list
          tourists <- catMaybes . V.toList <$> mapM (gotrs trsts_ids) trsts
          pure $ Just Hotel {..}
        else pure Nothing

    gotrs :: [Int] -> Object -> Parser (Maybe Tourist)
    gotrs ids obj = do
      t :: Int <- obj .: "u_id"
      if t `elem` ids
        then Just <$> parseTouristJSON obj
        else pure Nothing
