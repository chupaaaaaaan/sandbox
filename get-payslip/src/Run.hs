{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Control.Monad.Cont
import Import
import Scraper
import Util

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
        liftIO $ downloadZip baseDir content
  where
    Nothing !? e = ContT $ const $ logWarn e
    Just a !? _ = ContT ($ a)
