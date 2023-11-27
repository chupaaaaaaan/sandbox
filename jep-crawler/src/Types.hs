{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types (
    App (..),
    Options (..),
    Jep (..),
    Jdk (..),
) where

import Data.Csv (ToField (toField), ToRecord (toRecord), record)
import RIO
import RIO.Process
import qualified RIO.Text as T

-- | Command line arguments
data Options = Options
    { optionsVerbose :: !Bool
    , optionsOutputFilePath :: !String
    , optionsJdk :: !Jdk
    , optionsDoTranslate :: !Bool
    }

data App = App
    { appLogFunc :: !LogFunc
    , appProcessContext :: !ProcessContext
    , appOptions :: !Options
    }

instance HasLogFunc App where
    logFuncL = lens (\a -> a.appLogFunc) (\x y -> x{appLogFunc = y})
instance HasProcessContext App where
    processContextL = lens (\a -> a.appProcessContext) (\x y -> x{appProcessContext = y})

data Jep = Jep
    { title :: Text
    , url :: Text
    , component :: Text
    , summary :: [Text]
    , summaryJa :: [Text]
    }
    deriving (Show)

instance ToRecord Jep where
    toRecord (Jep{..}) =
        record
            [ toField title
            , toField url
            , toField component
            , toField (T.unwords summary)
            , toField (T.unwords summaryJa)
            ]

data Jdk = Jdk7 | Jdk8 | Jdk9 | Jdk10 | Jdk11 | Jdk12 | Jdk13 | Jdk14 | Jdk15 | Jdk16 | Jdk17
    deriving (Read)
