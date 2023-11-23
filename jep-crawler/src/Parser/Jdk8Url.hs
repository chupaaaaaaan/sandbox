{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser.Jdk8Url (
    jepUrlParser,
    jdkUrl
) where

import Import
import Text.HTML.Scalpel.Core
import Network.HTTP.Simple (Request)
import qualified RIO.Map as Map

jdkUrl :: Request
jdkUrl = "https://openjdk.org/projects/jdk8/features"

-- jepUrlParser :: Scraper Text [Text]
-- jepUrlParser = fmap join $ chroot ("div" @: ["id" @= "main"]) $ inSerial $ many $ do
--     t <- seekNext $ text "h1"
--     guard (t == "Features")
--     untilNext (matches "h1") (seekNext $ chroot ("blockquote" // "div" @: [hasClass "jep-summary"] // "table") $ inSerial $ many $ do
--                                      seekNext $ chroot "tr")
