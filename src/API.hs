{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.Text          (Text)
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5

import           Types

type FullAPI = StaticFiles :<|> (Docs :<|> Index :<|> API)
type API = Users :<|> Books :<|> Copies :<|> Rentals

--------------------------------------------------------------------------------

-- | Users

type Users = AddUser
        :<|> GetUsers :<|> GetUserById
        :<|> UpdateUser
        :<|> DeleteUser

-- Create
type AddUser     = "users" :> ReqBody '[JSON] AddUserRequest :> Post '[JSON] AddUserResponse
-- Read
type GetUsers    = "users" :> QueryParam "name" Name :> Get '[JSON] [User]
type GetUserById = "users" :> Capture "user_id" (InternalId User) :> Get '[JSON] User
-- Update
type UpdateUser  = "users" :> Capture "user_id" (InternalId User) :> ReqBody '[JSON] UpdateUserRequest :> Post '[JSON] UpdateUserResponse
-- Delete
type DeleteUser  = "users" :> Capture "user_id" (InternalId User) :> PostNoContent '[JSON] NoContent

--------------------------------------------------------------------------------

-- | Books

type Books = AddBook
        :<|> GetAllBooks :<|> GetBookByIsbn

-- Create
type AddBook       = "books" :> ReqBody '[JSON] Book :> PostNoContent '[JSON] NoContent
-- Read
type GetAllBooks   = "books"                             :> Get '[JSON] [Book]
type GetBookByIsbn = "books" :> Capture "book_isbn" ISBN :> Get '[JSON]  Book
-- Update
--type UpdateBook
-- Delete
--type DeleteBook

--------------------------------------------------------------------------------

-- | Copies

type Copies = AddCopy
         :<|> GetCopies
         :<|> UpdateCopy
         :<|> DeleteCopy

-- Create
type AddCopy    = "books"  :> Capture "book_isbn" ISBN :> "copies" :> ReqBody '[JSON] AddCopyRequest :> Post '[JSON] AddCopyResponse
-- Read
type GetCopies  = "books"  :> Capture "book_isbn" ISBN :> "copies" :> Get '[JSON] [Copy]
-- Update
type UpdateCopy = "copies" :> Capture "copy_id" (InternalId Copy) :> ReqBody '[JSON] UpdateCopyRequest :> PostNoContent '[JSON] NoContent
-- Delete
type DeleteCopy = "copies" :> Capture "copy_id" (InternalId Copy) :> DeleteNoContent '[JSON] NoContent

--------------------------------------------------------------------------------

-- | Rentals

type Rentals = RentCopy
          :<|> CompleteRental

-- Create
type RentCopy       = "rentals" :> ReqBody '[JSON] RentalRequest :> Post '[JSON] RentalResponse
-- Update
type CompleteRental = "rentals" :> "complete" :> ReqBody '[JSON] CompleteRentalRequest :> Post '[JSON] CompleteRentalResponse

-- | Extra
type Docs  = "docs.md" :> Get '[PlainText] Text
type StaticFiles = "static" :> Raw
type Index = Get '[HTML] Html
