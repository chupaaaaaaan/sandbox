{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser.JepContent (
    jepParser
) where

import Import
import qualified RIO.Map as Map
import Text.HTML.Scalpel.Core

jepParser :: Scraper Text Jep
jepParser = chroot ("div" @: ["id" @= "main"]) $ do
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
        Jep
            { _title = title
            , _component = rows Map.!? "Component"
            , _summary = mainLines Map.!? "Summary"
            }

