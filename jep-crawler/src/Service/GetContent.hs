{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.GetContent (
    jepContents,
) where

import Import
import Network.HTTP.Simple
import qualified RIO.Map as Map
import qualified RIO.Text as T
import Text.HTML.Scalpel.Core
import Util

jepContents :: Jdk -> [Text] -> RIO env [Jep]
jepContents jdk urls = do
    let parser = case jdk of
            Jdk7 -> jsrParser
            Jdk8 -> jepParser
            Jdk9 -> jepParser
            Jdk10 -> jepParser
            Jdk11 -> jepParser
            Jdk12 -> jepParser
            Jdk13 -> jepParser
            Jdk14 -> jepParser
            Jdk15 -> jepParser
            Jdk16 -> jepParser
            Jdk17 -> jepParser

    fmap join $ forM urls $ \url -> do
        mJep <- flip scrapeStringLike (parser url) <$> (fetchHtml . parseRequest_ $ T.unpack url)
        case mJep of
            Nothing -> error "empty content"
            Just jep -> return jep

jsrParser :: Text -> Scraper Text [Jep]
jsrParser url = fmap join $ chroot ("div" @: [hasClass "feature-details"]) $ do
    chroots ("div" @: [hasClass "group"]) $ do
        chroots ("div" @: [hasClass "feature"]) $ do
            title <- text ("div" @: [hasClass "title"])
            summary <- text ("div" @: [hasClass "summary"])
            return
                Jep
                    { title = title
                    , url = url
                    , component = ""
                    , summary = [summary]
                    , summaryJa = []
                    }

jepParser :: Text -> Scraper Text [Jep]
jepParser url = chroot ("div" @: ["id" @= "main"]) $ do
    title <- text "h1"
    rows <- fmap Map.fromList $ chroot ("table" @: [hasClass "head"]) $ inSerial $ many $ do
        seekNext $ chroot "tr" $ inSerial $ do
            name <- seekNext $ text "td"
            key <- seekNext $ text "td"
            return (name, key)
    mainLines <- fmap Map.fromList $ chroot ("div" @: [hasClass "markdown"]) $ inSerial $ many $ do
        section <- seekNext $ text "h2"
        ps <- untilNext (matches "h2") (many $ seekNext $ text "p")
        return (section, ps)

    return
        $ (: [])
            Jep
                { title = title
                , url = url
                , component = fromMaybe "" (rows Map.!? "Component")
                , summary = fromMaybe [] (mainLines Map.!? "Summary")
                , summaryJa = []
                }
