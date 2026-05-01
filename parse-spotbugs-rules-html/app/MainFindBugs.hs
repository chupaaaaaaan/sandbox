{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Lucid
import Text.HTML.Scalpel

data Item = Item
    { code :: T.Text
    , url :: T.Text
    }

main :: IO ()
main = do

    let url = "https://findbugs.sourceforge.net/bugDescriptions.html"

    result <- fromJust <$> scrapeURL url (itemScraper $ T.pack url)
    T.writeFile "findbugs-list.html" $ renderText $ page result

itemScraper :: T.Text -> Scraper T.Text [Item]
itemScraper _url = catMaybes <$> do
    chroot "td" $ inSerial $ do
        _ <- seekNext $ do
            t <- text "h2"
            guard (T.strip t == "Descriptions")
            pure t
        untilNext (matches "h2") $ many $ seekNext $ do
            _text <- spritMsg <$> text "h3"
            pure $ case _text of
                Just (_desc,_code) -> Just $ Item { code = _code, url = _url <> T.cons '#' _code}
                Nothing -> Nothing

spritMsg :: T.Text -> Maybe (T.Text, T.Text)
spritMsg txt =
    let t = T.strip txt
        (body, rest) = T.breakOnEnd "(" t
    in case T.stripSuffix ")" rest of
        Just code -> let desc = T.strip (T.dropEnd 1 body)
                     in Just (desc, code)
        Nothing -> Nothing

page :: [Item] -> Html ()
page rows = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ "Bug List"
    body_ $
      table_ [] $ do
        thead_ $
          tr_ $ do
            th_ "Link"
        tbody_ $
          mapM_ rowHtml rows

rowHtml :: Item -> Html ()
rowHtml Item{..} =
  tr_ $ do
    td_ $
      a_ [href_ $ T.toStrict url] (toHtml code)
