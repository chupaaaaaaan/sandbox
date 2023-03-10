{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Text
import Data.Time (UTCTime)
import Servant.API

type UserAPI = "users" :> QueryParam "sortBy" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: UTCTime
    }

type UserAPI2 =
    "users" :> "list-all" :> Get '[JSON] [User]
        :<|> "list-all" :> "users" :> Get '[JSON] [User]

type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]

type UserAPI4 =
    "users" :> Get '[JSON] [User]
        :<|> "admins" :> Get '[JSON] [User]

type UserAPI5 =
    "user" :> Capture "userid" Integer :> Get '[JSON] User
        :<|> "user" :> Capture "userid" Integer :> DeleteNoContent

type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

type UserAPI7 =
    "users" :> ReqBody '[JSON] User :> Post '[JSON] User
        :<|> "users"
            :> Capture "userid" Integer
            :> ReqBody '[JSON] User
            :> Put '[JSON] User

type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]

type UserAPI9 = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]

type UserAPI10 = "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])

type ProtectedAPI11 =
    UserAPI
        :<|> BasicAuth "my-realm" User :> UserAPI2

type UserAPI12 innerAPI =
    UserAPI
        :<|> "inner" :> innerAPI

type UserAPI12Alone = UserAPI12 EmptyAPI

type UserAPI13 =
    "users" :> Get '[JSON] [User]
        :<|> Raw

type UserAPI14 =
    Raw
        :<|> "users" :> Get '[JSON] [User]

type UserAPI15 =
    "files" :> Raw
        :<|> "users" :> Get '[JSON] [User]

type UserAPI16 =
    "users" :> Get '[JSON] [User]
        :<|> Raw
