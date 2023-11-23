{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser.Jdk9Url (
    jepUrlParser,
    jdkUrl
) where

import Import
import Text.HTML.Scalpel.Core
import Network.HTTP.Simple (Request)

jdkUrl :: Request
jdkUrl = "https://openjdk.org/projects/jdk9/"

jepUrlParser :: Scraper Text [Text]
jepUrlParser = fmap join $ chroot ("div" @: ["id" @= "main"]) $ inSerial $ many $ do
    t <- seekNext $ text "h2"
    guard (t == "Features")
    untilNext (matches "h2") (seekNext $ chroot "blockquote" $ inSerial $ many $ seekNext $ attr "href" "a")
