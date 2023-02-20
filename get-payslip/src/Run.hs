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
import Control.Monad.Cont

run :: (HasLogFunc env, HasProcessContext env) => RIO env ()
run = do

    mAppConfig <- getAppConfig
    (`runContT` return) $ do
        appConfig <- mAppConfig !? "Environment variable `GET_PAYSLIP_PASSWORD' is not set."
        let password = appConfig.targetUrlPassword
            host = appConfig.webdriverHost
            port = appConfig.webdriverPort
            baseDir = appConfig.downloadBaseDir
        mContent <- liftIO $ scrapeContent <$> fetchContentPage host port password
        content <- mContent !? "Cannot get the target content from the page."

        logInfo $ display content._url
        -- logInfo $ display content._title
        liftIO $ downloadZip baseDir content._url

    where
        Nothing !? e = ContT $ const $ logWarn e
        Just a  !? _ = ContT ($ a)
