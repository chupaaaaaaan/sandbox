{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import Network.HTTP.Simple
import Parser.JepContent
import qualified Parser.Jdk9Url as Jdk
import qualified RIO.Text as T
import Text.HTML.Scalpel.Core
import Util

run :: RIO App ()
run = do
    content <- fetchHtml Jdk.jdkUrl

    let maybeSourceUrls = scrapeStringLike content Jdk.jepUrlParser

    case maybeSourceUrls of
        Nothing -> error "URL is empty."
        Just sourceUrls -> do
            forM_ sourceUrls $ \su -> do
                jepContent <- fetchHtml $ parseRequest_ $ T.unpack su
                logInfo . displayShow $ scrapeStringLike jepContent jepParser
