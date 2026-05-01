{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Char
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Lucid
import Text.HTML.Scalpel
import qualified Data.Text.Lazy.Encoding as TE
import qualified Network.HTTP.Simple as HTTP
import qualified Data.ByteString.Lazy as BS

data Item = Item
    { code :: T.Text
    , url :: T.Text
    }

main :: IO ()
main = do

    let url = "https://spotbugs.readthedocs.io/en/latest/bugDescriptions.html"

    html' <- downloadHtml url
    writeFile "debug.txt" (T.unpack html')

    let res1 = scrapeStringLike html' $ htmls ("section" @: ["id" @= "bug-descriptions"])

        res2 = scrapeStringLike html' $ chroot ("section" @: ["id" @= "bug-descriptions"]) $ htmls "section"

        res3 = scrapeStringLike html' $ chroot ("section" @: ["id" @= "bug-descriptions"]) $ chroots "section" $ do
            pos <- position
            h3s <- htmls "h3"
            pure (pos, length h3s)

    print (length <$> res1, length <$> res2, res3)



    
    -- result <- fromJust <$> scrapeURL @T.Text url (itemScraper $ T.pack url)
    -- T.writeFile "spotbugs-list.html" $ renderText $ page result

downloadHtml :: String -> IO T.Text
downloadHtml url = do
  req <- HTTP.parseRequest url
  res <- HTTP.httpBS req
  pure $ TE.decodeUtf8 (BS.fromStrict $ HTTP.getResponseBody res)

itemScraper :: T.Text -> Scraper T.Text [Item]
itemScraper _url = catMaybes <$> do
    chroot ("section" @: ["id" @= "bug-descriptions"]) $ chroots "h3" $ do
        _text <- spritMsg <$> text "h3"
        _flagment <- attr "href" "a"
        pure $ case _text of
            Just (_desc, _code) -> Just $ Item {code = _code, url = _url <> _flagment}
            Nothing -> Nothing

removePUA :: T.Text -> T.Text
removePUA = T.filter (\c -> not (0xE000 <= ord c && ord c <= 0xF8FF))

spritMsg :: T.Text -> Maybe (T.Text, T.Text)
spritMsg txt =
    let t = T.strip (removePUA txt)
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
