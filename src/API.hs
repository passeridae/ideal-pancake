{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                (Text)
import           Data.UUID
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Types

type FullAPI = Docs :<|> Index :<|> API
type API = GetAllUsers :<|> GetAllBooks --GetUser :<|> GetAllBooks :<|> GetBook :<|> GetCopies :<|> RegisterBook :<|> AddCopy

-- | Users

type GetAllUsers  = "users"                                                                :> Get '[JSON] [User]
type GetUser      = "users" :> Capture "user_id" UUID                                      :> Get '[JSON] User
type DeleteUser   = "users" :> Capture "user_id" UUID :> ReqBody '[JSON] DeleteUserRequest :> Get '[JSON] DeleteUserResponse
type UpdateUser   = "users" :> Capture "user_id" UUID :> ReqBody '[JSON] UpdateUserRequest :> Get '[JSON] UpdateUserResponse
type AddUser      = "users"                           :> ReqBody '[JSON] AddUserRequest    :> Get '[JSON] AddUserResponse

-- | Books

type GetAllBooks  = "books"                                         :> Get '[JSON] [Book]
type GetBook      = "books" :> Capture "book_isbn" ISBN             :> Get '[JSON]  Book
type GetCopies    = "books" :> Capture "book_isbn" ISBN :> "copies" :> Get '[JSON] [Copy]
type RegisterBook = "books" :> ReqBody '[JSON] Book     :> PostNoContent '[JSON] ()

-- | Copies
type AddCopy      = "books" :> Capture "book_isbn" ISBN :> "copies" :> ReqBody '[JSON] AddCopyRequest :> Post '[JSON] UUID

-- | Extra
type Docs  = "docs.md" :> Get '[PlainText] Text
type Index = Get '[PlainText] Text
