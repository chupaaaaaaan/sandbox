{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types (
    App (..),
    Options (..),
    Content(..),
    AppConfig_ (..),
    getAppConfig,
) where

import Barbies
import Barbies.Bare
import Data.Monoid
import Data.Monoid.Generic
import RIO
import RIO.Map qualified as Map
import RIO.Process
import RIO.Text

data AppConfig_ t f = AppConfig_
    { targetUrlPassword :: Wear t f Text
    , webdriverHost :: Wear t f String
    , webdriverPort :: Wear t f Int
    , downloadBaseDir :: Wear t f FilePath
    }
    deriving (Generic)

instance FunctorB (AppConfig_ Covered)
instance TraversableB (AppConfig_ Covered)
instance ConstraintsB (AppConfig_ Covered)
instance BareB AppConfig_

deriving via GenericSemigroup (AppConfig_ Covered f) instance AllBF Semigroup f (AppConfig_ Covered) => Semigroup (AppConfig_ Covered f)
deriving via GenericMonoid (AppConfig_ Covered f) instance AllBF Monoid f (AppConfig_ Covered) => Monoid (AppConfig_ Covered f)

type AppConfigPartial = AppConfig_ Covered Last
type AppConfig = AppConfig_ Bare Identity

defaultAppConfig :: RIO env AppConfigPartial
defaultAppConfig =
    return $
        AppConfig_
            { targetUrlPassword = Last Nothing
            , webdriverHost = Last (Just "localhost")
            , webdriverPort = Last (Just 4444)
            , downloadBaseDir = Last (Just "./")
            }

appConfigFromEnvironment :: HasProcessContext env => RIO env AppConfigPartial
appConfigFromEnvironment = do
    envMap <- view envVarsL
    let lu = flip Map.lookup envMap
    return $
        AppConfig_
            { targetUrlPassword = Last $ lu "GET_PAYSLIP_PASSWORD"
            , webdriverHost = Last $ lu "GET_PAYSLIP_HOST" <&> unpack
            , webdriverPort = Last $ lu "GET_PAYSLIP_PORT" >>= readMaybe . unpack
            , downloadBaseDir = Last $ lu "GET_PAYSLIP_DOWNLOAD_BASEDIR" <&> unpack
            }

getAppConfig :: HasProcessContext env => RIO env (Maybe AppConfig)
getAppConfig = do
    fromDefault <- defaultAppConfig
    fromEnvironment <- appConfigFromEnvironment

    return . getLast . fmap bstrip . bsequence' $ fromDefault <> fromEnvironment

-- | download content
data Content = Content
    { _url :: !Text
    , _title :: !Text
    }
    deriving (Show)

-- | Command line arguments
data Options = Options
    { optionsVerbose :: Bool
    }

data App = App
    { appLogFunc :: LogFunc
    , appProcessContext :: ProcessContext
    , appOptions :: Options
    -- Add other app-specific configuration information here
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x{appLogFunc = y})
instance HasProcessContext App where
    processContextL = lens appProcessContext (\x y -> x{appProcessContext = y})
