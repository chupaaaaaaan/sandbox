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
        doTranslate = env.appOptions.optionsDoTranslate

    urlSourceOf targetJdk
        >>= jepContents targetJdk
        >>= \jeps -> (if doTranslate then translate2Ja jeps else return jeps)
        >>= writeToCsv outputFilePath
