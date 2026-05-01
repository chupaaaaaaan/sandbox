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
{-# LANGUAGE OverloadedRecordDot #-}

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
import Control.Monad.Fix
import Control.Arrow
import Data.Maybe (fromMaybe)

type Properties = M.Map T.Text T.Text

data CSModule = CSModule
    { name :: !T.Text
    , properties :: !Properties
    , children :: ![CSModule]
    }
    deriving (Show, Eq, Ord)

data InvCSModule = InvCSModule
    { name :: !T.Text
    , properties :: !(M.Map T.Text T.Text)
    , parentName :: !(Maybe T.Text)
    }
    deriving (Show, Eq, Ord)

type BugCollection = M.Map T.Text (S.Set BugInstance)
data BugInstance = BugInstance
    { type_ :: !T.Text
    , priority :: !T.Text
    , category :: !T.Text
    , message :: !T.Text
    , lineNumber :: !T.Text
    }
    deriving (Show, Eq, Ord)

toCSModule :: TX.Document -> CSModule
toCSModule = go . fromDocument
    where reqAttr :: Cursor -> TX.Name -> T.Text
          reqAttr cursor name = only $ attribute name cursor
          go :: Cursor -> CSModule
          go cur = let name = reqAttr cur "name"
                       properties = M.fromList
                           [ (reqAttr property "name", reqAttr property "value")
                           | property <- cur $/ element "property" ]
                       children = [ go child
                                  | child <- cur $/ element "module" ]
                   in CSModule name properties children



toInvRoot :: CSModule -> InvCSModule
toInvRoot csm = InvCSModule csm.name csm.properties Nothing

gn :: CSModule -> T.Text
gn csm = csm.name

gp :: CSModule -> Properties
gp csm = csm.properties

toInvCSModule :: CSModule -> [InvCSModule]
toInvCSModule csm = toInvRoot csm : flip fix csm \go v -> do
    let chs = map (gn &&& gp) v.children
        acc = map (f v) chs
        acc' = concatMap go v.children
    acc <> acc'
    where f :: CSModule -> (T.Text, Properties) -> InvCSModule
          f csm' (nm, pr) = InvCSModule { name = nm
                                        , properties = pr
                                        , parentName = Just csm'.name
                                        }

toRecords :: InvCSModule -> [C.NamedRecord]
toRecords InvCSModule{..} =
    [C.namedRecord [ "name" C..= name
                   , "parent" C..= pn
                   , "propertyName" C..= propName
                   , "propertyValue" C..= propVal
                   ]
    | pn <- [fromMaybe "" parentName]
    , (propName, propVal) <- M.toAscList properties
    ]

header :: C.Header
header = V.fromList [ "name"
                    , "parent"
                    , "propertyName"
                    , "propertyValue"
                    ]

toCsv :: [InvCSModule] -> BL.ByteString
toCsv = C.encodeByName header . concatMap toRecords

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fn] -> do
            doc <- TX.readFile TX.def fn
            BL.putStr $ toCsv $ toInvCSModule $ toCSModule doc
        _ -> error "Invalid argument number"

only :: [a] -> a
only = \case
    [x] -> x
    [] -> error "only: empty"
    _multiple -> error "only: multiple"
