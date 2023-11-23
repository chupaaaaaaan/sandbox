{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser.Jdk10Url (
    jepUrlParser,
    jdkUrl
) where

import Import
import Network.HTTP.Simple
import qualified Parser.Jdk9Url as Jdk9
import Text.HTML.Scalpel.Core

jdkUrl :: Request
jdkUrl = "https://openjdk.org/projects/jdk/10/"

jepUrlParser :: Scraper Text [Text]
jepUrlParser = Jdk9.jepUrlParser
