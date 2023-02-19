{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import Import
import Scraper
import Util
import RIO.Process
import qualified RIO.Map as Map
import Control.Monad.Cont
import RIO.Text (unpack)

run :: (HasLogFunc env, HasProcessContext env) => RIO env ()
run = do
    -- content <- scrapeContent <$> readFileUtf8 "./test.html"
    envMap <- view envVarsL

    let lu = flip Map.lookup envMap
        mPassword        = lu "GET_PAYSLIP_PASSWORD"
        mHost            = lu "GET_PAYSLIP_HOST" <&> unpack
        mPort            = lu "GET_PAYSLIP_PORT" >>= readMaybe . unpack
        mDownloadBaseDir = lu "GET_PAYSLIP_DOWNLOAD_BASEDIR" <&> unpack
    
    (`runContT` return) $ do 
        password <- mPassword !? "Environment variable `GET_PAYSLIP_PASSWORD' is not set."
        host <- mHost !? "Environment variable `GET_PAYSLIP_HOST' is not set."
        port <- mPort !? "Environment variable `GET_PAYSLIP_PORT' is not set or is not Int."
        downloadBaseDir <- mDownloadBaseDir !? "Environment variable `GET_PAYSLIP_DOWNLOAD_BASEDIR' is not set or is not Int."
        mContent <- liftIO $ scrapeContent <$> fetchContentPage host port password
        content <- mContent !? "Cannot get the target content from the page."

        logInfo $ display content._url
        -- logInfo $ display content._title
        liftIO $ downloadZip downloadBaseDir content._url

    where
        Nothing !? e = ContT $ const $ logWarn e
        Just a  !? _ = ContT ($ a)
