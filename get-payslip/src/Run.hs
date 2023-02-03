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

run :: (HasLogFunc env, HasProcessContext env) => RIO env ()
run = do
    -- content <- scrapeContent <$> readFileUtf8 "./test.html"
    envMap <- view envVarsL

    let envName = "GET_PAYSLIP_PASSWORD"
        mPassword = Map.lookup envName envMap
    
    (`runContT` return) $ do 
        password <- mPassword !? ("Environment variable '" <> display envName <> "` is not set.")
        mContent <- liftIO $ scrapeContent <$> fetchContentPage password
        content <- mContent !? "Cannot get the target content from the page."

        logInfo $ display content._url
        logInfo $ display content._title
        liftIO $ downloadZip content._url

    where
        Nothing !? e = ContT $ const $ logWarn e
        Just a  !? _ = ContT ($ a)
