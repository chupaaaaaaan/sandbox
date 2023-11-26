{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Service.UrlSource (
    urlSourceOf
) where

import Import
import Text.HTML.Scalpel.Core
import Util

urlSourceOf :: Jdk -> RIO env [Text]
urlSourceOf jdk = do
    let (sourceUrl, parser) = case jdk of
            Jdk7 -> ("", undefined)
            Jdk8 -> ("https://openjdk.org/projects/jdk8/features", jepUrlParserForJdk8)
            Jdk9 -> ("https://openjdk.org/projects/jdk9/", jepUrlParserForJdk9OrLater)
            Jdk10 -> ("https://openjdk.org/projects/jdk/10/", jepUrlParserForJdk9OrLater)
            Jdk11 -> ("https://openjdk.org/projects/jdk/11/", jepUrlParserForJdk9OrLater)
            Jdk12 -> ("https://openjdk.org/projects/jdk/12/", jepUrlParserForJdk12OrLater)
            Jdk13 -> ("https://openjdk.org/projects/jdk/13/", jepUrlParserForJdk12OrLater)
            Jdk14 -> ("https://openjdk.org/projects/jdk/14/", jepUrlParserForJdk12OrLater)
            Jdk15 -> ("https://openjdk.org/projects/jdk/15/", jepUrlParserForJdk12OrLater)
            Jdk16 -> ("https://openjdk.org/projects/jdk/16/", jepUrlParserForJdk12OrLater)
            Jdk17 -> ("https://openjdk.org/projects/jdk/17/", jepUrlParserForJdk12OrLater)

    case jdk of
        Jdk7 -> return ["https://openjdk.org/projects/jdk7/features/"]
        _ -> fromMaybe [] . flip scrapeStringLike parser <$> fetchHtml sourceUrl

jepUrlParserForJdk8 :: Scraper Text [Text]
jepUrlParserForJdk8 = fmap join $ chroot ("div" @: ["id" @= "main"]) $ inSerial $ many $ do
    t <- seekNext $ text "h1"
    guard (t == "Features")
    untilNext (matches "h1") $ seekNext $ chroot ("blockquote" // "div" @: [hasClass "jep-summary"] // "table") $ inSerial $ many $ do
        seekNext $ chroot "tr" $ inSerial $ do
            seekNext $ text "td"
            url <- seekNext $ chroot "td" $ attr "href" "a"
            seekNext $ text "td"
            return url

jepUrlParserForJdk9OrLater :: Scraper Text [Text]
jepUrlParserForJdk9OrLater = fmap join $ chroot ("div" @: ["id" @= "main"]) $ inSerial $ many $ do
    t <- seekNext $ text "h2"
    guard (t == "Features")
    untilNext (matches "h2") (seekNext $ chroot "blockquote" $ inSerial $ many $ seekNext $ attr "href" "a")

jepUrlParserForJdk12OrLater :: Scraper Text [Text]
jepUrlParserForJdk12OrLater = fmap join $ chroot ("div" @: ["id" @= "main"]) $ inSerial $ many $ do
    t <- seekNext $ text "h2"
    guard (t == "Features")
    untilNext (matches "h2") $ seekNext $ chroot ("blockquote" // "table" @: [hasClass "jeps"]) $ inSerial $ many $ do
        seekNext $ chroot "tr" $ inSerial $ do
            seekNext $ text "td"
            seekNext $ chroot "td" $ attr "href" "a"
            
