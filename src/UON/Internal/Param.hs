module UON.Internal.Param (Param (..), paramsToQuery) where

import Network.HTTP.Req (FormUrlEncodedParam)

class Param a where
  toQuery :: a -> FormUrlEncodedParam

paramsToQuery :: (Param a, Foldable t) => t a -> FormUrlEncodedParam
paramsToQuery = foldr ((<>) . toQuery) mempty