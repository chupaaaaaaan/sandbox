{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude.Compat
import Prelude ()

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Parser qualified
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Types.SourceT (source)
import System.Directory
import Text.Blaze
import Text.Blaze.Html qualified
import Text.Blaze.Html.Renderer.Utf8

type UserAPI1 = "users" :> Get '[JSON] [User]

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    }
    deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
    [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
    , User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
    ]

server1 :: Server UserAPI1
server1 = return users1

userAPI1 :: Proxy UserAPI1
userAPI1 = Proxy

app1 :: Application
app1 = serve userAPI1 server1

type UserAPI2 =
    "users" :> Get '[JSON] [User]
        :<|> "albert" :> Get '[JSON] User
        :<|> "isaac" :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server2 :: Server UserAPI2
server2 =
    return users2
        :<|> return albert
        :<|> return isaac

userAPI2 :: Proxy UserAPI2
userAPI2 = Proxy

app2 :: Application
app2 = serve userAPI2 server2

type API =
    "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
        :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
        :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    }
    deriving (Generic)

instance ToJSON Position

newtype HelloMessage = HelloMessage {msg :: String}
    deriving (Generic)

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
    { clientName :: String
    , clientEmail :: String
    , clientAge :: Int
    , clientInterestedIn :: [String]
    }
    deriving (Generic)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
    { from :: String
    , to :: String
    , subject :: String
    , body :: String
    }
    deriving (Generic)

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "great@company.com"
    to' = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body' =
        "Hi "
            ++ clientName c
            ++ ",\n\n"
            ++ "Since you've recently turned "
            ++ show (clientAge c)
            ++ ", have you checked out our latest "
            ++ intercalate ", " (clientInterestedIn c)
            ++ " products? Give us a visit!"

server3 :: Server API
server3 =
    position
        :<|> hello
        :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = return (Position x y)

    hello :: Maybe String -> Handler HelloMessage
    hello mname = return . HelloMessage $ case mname of
        Nothing -> "Hello, anonymous coward"
        Just n -> "Hello, " ++ n

    marketing :: ClientInfo -> Handler Email
    marketing clientinfo = return (emailForClient clientinfo)


api3 :: Proxy API
api3 = Proxy

app3 :: Application
app3 = serve api3 server3

runApp :: IO ()
runApp = run 8081 app3
