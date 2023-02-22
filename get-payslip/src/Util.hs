{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util (
    fetchContentPage,
    downloadZip,
    urlToFile,
) where

import Conduit
import Data.Aeson
import Network.HTTP.Simple
import Import
import RIO.Directory
import RIO.FilePath
import RIO.List qualified as L
import RIO.Text qualified as T
import RIO.Vector qualified as V
import Test.WebDriver

fetchContentPage :: String -> Int -> Text -> IO Text
fetchContentPage host port password = runSession config . finallyClose $ do
    let targetUrl = "https://kyuyokaitori.com/481"

    setImplicitWait 10000
    openPage targetUrl
    pwInput <- findElem (ByCSS "input[id='pwbox-481']")
    sendKeys password pwInput
    pwSubmit <- findElem (ByCSS "input[type='submit']")
    click pwSubmit
    getSource
  where
    -- https://stackoverflow.com/questions/57846224/haskell-webdriver-selenium-firefox-headless-mode
    config :: WDConfig
    config =
        defaultConfig
            { wdHost = host
            , wdPort = port
            , wdCapabilities =
                defaultCaps
                    { additionalCaps = [("moz:firefoxOptions", object [("args", Array (V.fromList [String "--headless"]))])]
                    }
            }

downloadZip :: (MonadUnliftIO m, MonadThrow m) => FilePath -> Content -> m ()
downloadZip baseDir content = runConduitRes $ do
    let (dname, fname) = urlToFile content._url
    req <- parseRequest $ T.unpack content._url
    createDirectoryIfMissing True dname
    let basename = takeBaseName fname
        extension = takeExtension fname
        filename = basename <> "_" <> T.unpack content._title <> extension
    httpSource req getResponseBody .| sinkFile (baseDir </> dname </> filename)

urlToFile :: Text -> (FilePath, String)
urlToFile =
    let urlPrefix = "https://kyuyokaitori.com/wp-content/"
     in pair (takeDirectory, takeFileName) . L.dropPrefix urlPrefix . T.unpack
  where
    pair :: (a -> b, a -> c) -> a -> (b, c)
    pair (f, g) x = (f x, g x)
