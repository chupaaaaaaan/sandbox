{-# LANGUAGE NoImplicitPrelude #-}
module Util
  ( fetchHtml
  ) where

import Import
import Network.HTTP.Simple
import Data.ByteString.Lazy as B

-- Download utility
fetchHtml :: Request -> RIO env Text
fetchHtml req = httpLbs req <&> decodeUtf8With lenientDecode . B.toStrict . getResponseBody

-- downloadRequest :: String -> Request
-- downloadRequest host
--     = setRequestHost host
--     $ setRequestMethod "GET"
--     $ setRequestSecure True
--     $ defaultRequest
