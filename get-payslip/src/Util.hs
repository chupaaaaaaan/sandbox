{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
  ( fetchContentPage
  , downloadZip
  , urlToFile
  ) where

import RIO
import Test.WebDriver
import Data.Aeson
import Network.HTTP.Simple
import Conduit
import RIO.Directory
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified RIO.Vector as V

fetchContentPage :: Text -> IO Text
fetchContentPage password = runSession config . finallyClose $ do
    let targetUrl = "https://kyuyokaitori.com/481"
    
    setImplicitWait 10000
    openPage targetUrl
    pwInput <- findElem (ByCSS "input[id='pwbox-481']")
    sendKeys password pwInput
    pwSubmit <- findElem (ByCSS "input[type='submit']")
    click pwSubmit
    getSource

    -- https://stackoverflow.com/questions/57846224/haskell-webdriver-selenium-firefox-headless-mode
    where config :: WDConfig
          config = defaultConfig
              { wdCapabilities = defaultCaps
                  { additionalCaps = [ ("moz:firefoxOptions", object [ ("args", Array (V.fromList [String "--headless"])) ]) ]
                  }
              }


downloadZip :: (MonadUnliftIO m, MonadThrow m) => Text -> m ()
downloadZip url = runConduitRes $ do
    let (dname, fname) = urlToFile url
    req <- parseRequest $ T.unpack url
    createDirectoryIfMissing True dname
    httpSource req getResponseBody .| sinkFile (dname </> fname)


urlToFile :: Text -> (FilePath, String)
urlToFile = let urlPrefix = "https://kyuyokaitori.com/wp-content/"
            in pair (takeDirectory, takeFileName) . L.dropPrefix urlPrefix . T.unpack
    where pair :: (a -> b, a -> c) -> a -> (b, c)
          pair (f, g) x = (f x, g x)
