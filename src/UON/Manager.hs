module UON.Manager (Manager (..)) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)
import UON.Internal.Utils (textWithHtmlToText)
import UON.Office (Office)
import UON.Types (Id (..))

data Manager = Manager
  { uid :: Id Manager,
    office_id :: Maybe (Id Office),
    name :: Text,
    surname :: Text,
    active :: Bool
  }
  deriving (Show)

instance FromJSON Manager where
  parseJSON = withObject "Manager" $ \obj -> do
    uid <- obj .: "u_id"
    office_id <- obj .: "office_id"
    name <- obj .: "u_name"
    surname <- obj .: "u_surname"
    act <- obj .: "active"
    pure $
      Manager
        { uid = uid,
          office_id =
            if office_id > 0
              then Just (Id office_id)
              else Nothing,
          name = textWithHtmlToText name,
          surname = textWithHtmlToText surname,
          active = act == (1 :: Int)
        }
