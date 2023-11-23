{-# LANGUAGE NoImplicitPrelude #-}

module Types (
    App (..),
    Options (..),
    Jep (..),
) where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
    { optionsVerbose :: !Bool
    }

data App = App
    { appLogFunc :: !LogFunc
    , appProcessContext :: !ProcessContext
    , appOptions :: !Options
    -- Add other app-specific configuration information here
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x{appLogFunc = y})
instance HasProcessContext App where
    processContextL = lens appProcessContext (\x y -> x{appProcessContext = y})

data Jep = Jep
    { _title :: Text
    , _component :: Maybe Text
    , _summary :: Maybe [Text]
    }
    deriving (Show)
