{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

module Types (
    App (..),
    Options (..),
    AppConfig (..),
    getAppConfig,
) where

import Barbies
import Barbies.Bare
import Data.Monoid
import RIO
import RIO.Map qualified as Map
import RIO.Process
import RIO.Text
import Data.Semigroup.Generic
import Data.Monoid.Generic

data AppConfig t f = AppConfig_
    { targetUrlPassword :: Wear t f Text
    , webdriverHost :: Wear t f String
    , webdriverPort :: Wear t f Int
    , downloadBaseDir :: Wear t f FilePath
    }
    deriving (Generic)

instance FunctorB (AppConfig Covered)
instance TraversableB (AppConfig Covered)
instance ConstraintsB (AppConfig Covered)
instance BareB AppConfig

deriving via GenericSemigroup (AppConfig Covered f) instance AllBF Semigroup f (AppConfig Covered) => Semigroup (AppConfig Covered f)
deriving via GenericMonoid (AppConfig Covered f) instance AllBF Monoid f (AppConfig Covered) => Monoid (AppConfig Covered f)

defaultAppConfig :: RIO env (AppConfig Covered Last)
defaultAppConfig =
    return $
        AppConfig_
            { targetUrlPassword = Last Nothing
            , webdriverHost = Last (Just "localhost")
            , webdriverPort = Last (Just 4444)
            , downloadBaseDir = Last (Just "./")
            }

appConfigFromEnvironment :: HasProcessContext env => RIO env (AppConfig Covered Last)
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

getAppConfig :: HasProcessContext env => RIO env (Maybe (AppConfig Covered Identity))
getAppConfig = do
    fromDefault <- defaultAppConfig
    fromEnvironment <- appConfigFromEnvironment

    return $ getLast $ bsequence' $ fromDefault <> fromEnvironment

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
