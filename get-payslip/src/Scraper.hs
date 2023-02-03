{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Scraper (scrapeContent, Content(..))  where

import Import
import Text.HTML.Scalpel.Core
import qualified RIO.List.Partial as L'


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
               } deriving (Show)
