{-# LANGUAGE RecordWildCards #-}

module UON.ExtendedFields (list, ExtField (..)) where

import Data.Aeson (FromJSON (parseJSON), withObject, withScientific, (.:))
import Data.Text qualified as T
import GHC.Generics (Generic)
import UON qualified
import UON.Internal.Request qualified as Request

newtype R = R {records :: [ExtField]} deriving (Generic)

instance FromJSON R

data ExtField = ExtField
  { _id :: Int,
    section :: Section,
    name :: T.Text,
    options :: Maybe [T.Text],
    active :: Bool
  }
  deriving (Show)

data Section
  = Lead
  | Request
  | Client
  | Service
  | ClientPayment
  | SupplierPayment
  | OtherPayment
  | Supplier
  deriving (Show)

instance FromJSON Section where
  parseJSON = withScientific "section" $ \t -> intToSection t
    where
      intToSection 1 = pure Lead
      intToSection 2 = pure Request
      intToSection 3 = pure Client
      intToSection 4 = pure Service
      intToSection 5 = pure ClientPayment
      intToSection 6 = pure SupplierPayment
      intToSection 7 = pure OtherPayment
      intToSection 8 = pure Supplier
      intToSection x = fail $ "unknown section " <> show x

instance FromJSON ExtField where
  parseJSON = withObject "ext_field" $ \o -> do
    _id <- o .: "id"
    section <- o .: "section"
    name <- o .: "name"
    options <- o .: "options"
    active <- intToBool <$> o .: "active"
    pure $ ExtField {..}

intToBool :: Int -> Bool
intToBool 1 = True
intToBool 0 = False
intToBool x = error $ "not bool: " <> show x

list :: UON.Key -> Int -> IO (Either T.Text [ExtField])
list key page = do
  let p = T.pack $ show page
  a <- Request.get ["extended_field", p] key mempty
  pure $ case a of
    Request.Ok R {records = r} -> Right r
    Request.Status 404 _ -> Right []
    Request.Status _ t -> Left t
    Request.Error t -> Left t