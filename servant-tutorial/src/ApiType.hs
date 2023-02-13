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
