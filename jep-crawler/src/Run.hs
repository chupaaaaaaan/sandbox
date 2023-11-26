{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Run (run) where

import Import
import Service.GetContent
import Service.Translate
import Service.UrlSource
import Service.Output

run :: RIO App ()
run = do
    env <- ask
    let targetJdk = env.appOptions.optionsJdk
        outputFilePath = env.appOptions.optionsOutputFilePath

    urlSourceOf targetJdk
        >>= jepContents targetJdk
        >>= translate2Ja
        >>= writeToCsv outputFilePath
