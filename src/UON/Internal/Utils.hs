{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module UON.Internal.Utils
  ( textWithHtmlToText,
    intListToString,
    utcToUonTime,
    readDateTime,
    nonEmptyText,
  )
where

import Data.Aeson.Types (Key, Object, Parser, (.:?))
import Data.Text (Text, intercalate, pack)
import Data.Text qualified as T
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Time
  ( LocalTime,
    defaultTimeLocale,
    formatTime,
    parseTimeM,
  )
import HTMLEntities.Decoder (htmlEncodedText)

textWithHtmlToText :: Text -> Text
textWithHtmlToText = toStrict . toLazyText . htmlEncodedText

readDateTime :: Object -> Key -> Parser (Maybe LocalTime)
readDateTime obj key = do
  obj .:? key
    >>= pure . \case
      Nothing -> Nothing
      Just x -> parseTimeM True defaultTimeLocale "%F %R" x

utcToUonTime :: LocalTime -> String
utcToUonTime = formatTime defaultTimeLocale "%F %R"

intListToString :: [Int] -> Text
intListToString is = intercalate "," (map (pack . show) is)

nonEmptyText :: Object -> Key -> Parser (Maybe T.Text)
nonEmptyText obj key = do
  obj .:? key
    >>= pure . fmap T.strip
    >>= pure . \case
      Just txt
        | T.null (T.strip txt) -> Nothing
        | otherwise -> Just $ T.strip $ textWithHtmlToText txt
      Nothing -> Nothing
