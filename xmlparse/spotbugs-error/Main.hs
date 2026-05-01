#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , bytestring
             , cassava
             , containers
             , text
             , vector
             , xml-conduit
-}
{- project:
import: https://www.stackage.org/lts-23.28/cabal.config
with-compiler: ghc-9.8.4
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment
import qualified Text.XML as TX
import Text.XML.Cursor

type BugCollection = M.Map T.Text (S.Set BugInstance)
data BugInstance = BugInstance
    { type_ :: !T.Text
    , priority :: !T.Text
    , category :: !T.Text
    , message :: !T.Text
    , lineNumber :: !T.Text
    }
    deriving (Show, Eq, Ord)

toBugCollection :: TX.Document -> BugCollection
toBugCollection doc =
    M.fromList [(reqAttr file "classname", S.fromList $ map parseBugInstance $ file $/ element "BugInstance")
               | file <- fromDocument doc $/ element "file" ]

    where reqAttr :: Cursor -> TX.Name -> T.Text
          reqAttr cursor name = only $ attribute name cursor
          parseBugInstance :: Cursor -> BugInstance
          parseBugInstance bi = BugInstance
              (reqAttr bi "type")
              (reqAttr bi "priority")
              (reqAttr bi "category")
              (reqAttr bi "message")
              (reqAttr bi "lineNumber")

toRecords :: BugCollection -> [C.NamedRecord]
toRecords mp = [ C.namedRecord [ "classname" C..= classname
                               , "type" C..= type_
                               , "priority" C..= priority
                               , "category" C..= category
                               , "lineNumber" C..= lineNumber
                               , "message" C..= message
                               ]
            | (classname, bis) <- M.toAscList mp
            , BugInstance{..} <- S.toAscList bis ]

header :: C.Header
header = V.fromList [ "classname"
                    , "type"
                    , "priority"
                    , "category"
                    , "lineNumber"
                    , "message"
                    ]

toCsv :: BugCollection -> BL.ByteString
toCsv = C.encodeByName header . toRecords

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fn] -> do
            doc <- TX.readFile TX.def fn
            BL.putStr $ toCsv $ toBugCollection doc
        _ -> error "Invalid argument number"

only :: [a] -> a
only = \case
    [x] -> x
    [] -> error "only: empty"
    _multiple -> error "only: multiple"
