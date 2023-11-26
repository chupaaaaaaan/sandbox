{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util (
    fetchHtml,
    translate,
) where

import Import
import Network.HTTP.Simple
import Network.HTTP.Types
import qualified RIO.ByteString.Lazy as BL
import RIO.Process
import qualified RIO.Text as T

-- Download utility
fetchHtml :: Request -> RIO env Text
fetchHtml req = httpLBS req <&> decodeUtf8With lenientDecode . BL.toStrict . replace 0x0a 0x20 . getResponseBody

-- DeepL Translation
translate :: (HasLogFunc env, HasProcessContext env) => Text -> [Text] -> RIO env DeepLResponse
translate target texts = do
    let requestBody =
            DeepLRequest
                { text = texts
                , target_lang = target
                }

    deeplAuthKey <-
        lookupEnvFromContext "DEEPL_AUTH_KEY" <&> \case
            Nothing -> error "cannot find environment variable `DEEPL_AUTH_KEY'"
            Just key -> key

    responseEither <- withExponentialBackOff 9 (isRight . getResponseBody) (httpJSONEither (mkRequest requestBody $ T.encodeUtf8 deeplAuthKey))
    case getResponseBody responseEither of
        Left jsonException -> error $ show jsonException
        Right result -> return result
  where
    mkRequest :: DeepLRequest -> ByteString -> Request
    mkRequest body key =
        parseRequest_ "POST https://api-free.deepl.com/v2/translate"
            & setRequestHeader hAuthorization ["DeepL-Auth-Key " <> key]
            & setRequestHeader hContentType ["application/json"]
            & setRequestBodyJSON body

withExponentialBackOff :: forall env a. (HasLogFunc env) => Int -> (a -> Bool) -> RIO env a -> RIO env a
withExponentialBackOff maxCount isOk action = go 1 0
  where
    go :: Int -> Int -> RIO env a
    go ws cnt = do
        result <- action
        if isOk result || cnt >= maxCount
            then do
                unless (isOk result) $ logWarn $ displayShow maxCount <> " retries are all failed."
                return result
            else do
                logWarn $ "backoff count: " <> displayShow cnt <> ", waiting " <> displayShow ws <> " seconds..."
                threadDelay (ws * 1000 * 1000)
                go (ws * 2) (cnt + 1)

replace :: Word8 -> Word8 -> BL.ByteString -> BL.ByteString
replace old new = flip BL.foldr BL.empty $ \a bs -> if a == old then new `BL.cons` bs else a `BL.cons` bs
