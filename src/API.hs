{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.Text (Text)
import           Data.UUID
import           Servant

import           Types

type FullAPI = Docs :<|> Index :<|> API
type API = GetAllUsers :<|> AddUser
      :<|> GetAllBooks :<|> AddBook
      :<|> AddCopy --GetUser :<|> GetAllBooks :<|> GetBook :<|> GetCopies :<|> RegisterBook :<|> AddCopy

-- | Users

type GetAllUsers  = "users"                                                                :> Get '[JSON] [User]
type GetUser      = "users" :> Capture "user_id" UUID                                      :> Get '[JSON] User
type DeleteUser   = "users" :> Capture "user_id" UUID :> ReqBody '[JSON] DeleteUserRequest :> Get '[JSON] DeleteUserResponse
type UpdateUser   = "users" :> Capture "user_id" UUID :> ReqBody '[JSON] UpdateUserRequest :> Get '[JSON] UpdateUserResponse
type AddUser      = "users"                           :> ReqBody '[JSON] AddUserRequest    :> Post '[JSON] AddUserResponse

-- | Books

type GetAllBooks = "books"                             :> Get '[JSON] [Book]
type GetBook     = "books" :> Capture "book_isbn" ISBN :> Get '[JSON]  Book
type AddBook     = "books" :> ReqBody '[JSON] Book     :> PostNoContent '[JSON] NoContent

-- | Copies
type GetCopies   = "books" :> Capture "book_isbn" ISBN :> "copies" :> Get '[JSON] [Copy]
type AddCopy     = "books" :> Capture "book_isbn" ISBN :> "copies" :> ReqBody '[JSON] AddCopyRequest :> Post '[JSON] AddCopyResponse

-- | Extra
type Docs  = "docs.md" :> Get '[PlainText] Text
type Index = Get '[PlainText] Text
