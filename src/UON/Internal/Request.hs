{-# LANGUAGE DataKinds #-}

module UON.Internal.Request (get, newKey, Key, post, Answer (..)) where

import Control.Exception (try)
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client qualified as L
import Network.HTTP.Req
  ( GET (GET),
    HttpException (VanillaHttpException),
    JsonResponse,
    NoReqBody (NoReqBody),
    Option,
    POST (POST),
    ReqBodyUrlEnc (..),
    Scheme (Https),
    Url,
    defaultHttpConfig,
    https,
    jsonResponse,
    req,
    responseBody,
    responseStatusCode,
    responseStatusMessage,
    runReq,
    (/:),
  )
import UON.Internal.Param (Param, paramsToQuery)

api :: T.Text
api = "api.u-on.ru"

newtype Key = Key T.Text

newKey :: T.Text -> Key
newKey = Key

instance Show Key where
  show _ = "UON key hidden"

data Answer a
  = Error T.Text
  | Status Int T.Text
  | Ok a
  deriving (Show)

get ::
  (A.FromJSON b) =>
  [T.Text] ->
  Key ->
  Option 'Https ->
  IO (Answer b)
get path key query = doreq r
  where
    r :: (A.FromJSON b) => IO (JsonResponse b)
    r =
      runReq defaultHttpConfig $
        req
          GET
          (makePath key path)
          NoReqBody
          jsonResponse
          query

post ::
  (A.FromJSON b, Param p) =>
  [T.Text] ->
  Key ->
  [p] ->
  IO (Answer b)
post path key form = doreq r
  where
    r :: (A.FromJSON b) => IO (JsonResponse b)
    r =
      runReq defaultHttpConfig $
        req
          POST
          (makePath key path)
          (ReqBodyUrlEnc (paramsToQuery form))
          jsonResponse
          mempty

doreq :: (A.FromJSON b) => IO (JsonResponse b) -> IO (Answer b)
doreq r = do
  a <- try (responseBody <$> r)
  pure $ case a of
    Left e -> do
      case isStatusException e of
        Just (response, bs) ->
          Status
            (responseStatusCode response)
            (T.intercalate " | " (map TE.decodeUtf8 [responseStatusMessage response, bs]))
        Nothing -> Error $ T.pack (show e)
    Right x -> Ok x

makePath :: Key -> [T.Text] -> Url 'Https
makePath (Key key) path =
  let end = T.concat [last path, ".json"]
      start = init path
   in foldl (/:) (https api) $ [key] ++ start ++ [end]

isStatusException :: HttpException -> Maybe (L.Response (), BS.ByteString)
isStatusException
  ( VanillaHttpException
      ( L.HttpExceptionRequest
          _
          (L.StatusCodeException r bs)
        )
    ) = Just (r, bs)
isStatusException _ = Nothing