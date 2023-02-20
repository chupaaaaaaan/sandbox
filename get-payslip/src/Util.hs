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
import RIO
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

downloadZip :: (MonadUnliftIO m, MonadThrow m) => FilePath -> Text -> m ()
downloadZip baseDir url = runConduitRes $ do
    let (dname, fname) = urlToFile url
    req <- parseRequest $ T.unpack url
    createDirectoryIfMissing True dname
    httpSource req getResponseBody .| sinkFile (baseDir </> dname </> fname)

urlToFile :: Text -> (FilePath, String)
urlToFile =
    let urlPrefix = "https://kyuyokaitori.com/wp-content/"
     in pair (takeDirectory, takeFileName) . L.dropPrefix urlPrefix . T.unpack
  where
    pair :: (a -> b, a -> c) -> a -> (b, c)
    pair (f, g) x = (f x, g x)
