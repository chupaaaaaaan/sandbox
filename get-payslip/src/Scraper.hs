{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scraper (scrapeContent) where

import Import
import RIO.List.Partial qualified as L'
import Text.HTML.Scalpel.Core

scrapeContent :: Text -> Maybe Content
scrapeContent page = scrapeStringLike page contentScraper

contentScraper :: Scraper Text Content
contentScraper = chroot ("div" @: ["itemProp" @= "mainEntityOfPage"]) $ do
    url <- attr "href" "a"
    paragraphs <- texts "p"
    return $ Content url $ paragraphs L'.!! 1
