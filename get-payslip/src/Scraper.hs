{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scraper (scrapeContent, Content (..)) where

import RIO
import RIO.List.Partial qualified as L'
import Text.HTML.Scalpel.Core

scrapeContent :: Text -> Maybe Content
scrapeContent page = scrapeStringLike page contentScraper

contentScraper :: Scraper Text Content
contentScraper = chroot ("div" @: ["itemProp" @= "mainEntityOfPage"]) $ do
    url <- attr "href" "a"
    paragraphs <- texts "p"
    return $ Content url $ paragraphs L'.!! 1

data Content = Content
    { _url :: !Text
    , _title :: !Text
    }
    deriving (Show)
