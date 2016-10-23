{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.Text          (Text)
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5

import           Types

type FullAPI = (Docs :<|> Index :<|> API) :<|> StaticFiles
type API = Users :<|> Books :<|> Copies :<|> Rentals

--------------------------------------------------------------------------------

-- | Users

type Users = AddUser
        :<|> GetUsers :<|> GetUserById
        :<|> DeleteUser

-- Create
type AddUser     = "users" :> ReqBody '[JSON] AddUserRequest :> Post '[JSON] AddUserResponse
-- Read
type GetUsers    = "users" :> QueryParam "name" Name :> Get '[JSON] [User]
type GetUserById = "users" :> Capture "user_id" (InternalId User) :> Get '[JSON] User
-- Delete
type DeleteUser  = "users" :> Capture "user_id" (InternalId User) :> PostNoContent '[JSON] NoContent

--------------------------------------------------------------------------------

-- | Books

type Books = AddBook
        :<|> GetBooks :<|> GetBookByIsbn
        :<|> DeleteBook

-- Create
type AddBook       = "books" :> ReqBody '[JSON] Book :> PostNoContent '[JSON] NoContent
-- Read
type GetBooks      = "books" :> QueryParam "title" Name  :> Get '[JSON] [Book]
type GetBookByIsbn = "books" :> Capture "book_isbn" ISBN :> Get '[JSON]  Book
-- Update
--type UpdateBook
-- Delete
type DeleteBook    = "books" :> Capture "book_isbn" ISBN :> PostNoContent '[JSON] NoContent

--------------------------------------------------------------------------------

-- | Copies

type Copies = AddCopy
         :<|> GetCopies
         :<|> GetCopyById
         :<|> UpdateCopy
         :<|> DeleteCopy

-- Create
type AddCopy    = "books"  :> Capture "book_isbn" ISBN :> "copies" :> ReqBody '[JSON] AddCopyRequest :> Post '[JSON] AddCopyResponse
-- Read
type GetCopies  = "books"  :> Capture "book_isbn" ISBN :> "copies" :> Get '[JSON] [Copy]
type GetCopyById = "copies" :> Capture "copy_id" (InternalId Copy) :> Get '[JSON] Copy
-- Update
type UpdateCopy = "copies" :> Capture "copy_id" (InternalId Copy) :> ReqBody '[JSON] UpdateCopyRequest :> PostNoContent '[JSON] NoContent
-- Delete
type DeleteCopy = "copies" :> Capture "copy_id" (InternalId Copy) :> DeleteNoContent '[JSON] NoContent

--------------------------------------------------------------------------------

-- | Rentals

type Rentals = RentCopy
          :<|> CompleteRental :<|> GetRentalsByUser
          :<|> GetRentalByCopy

-- Create
type RentCopy         = "rentals" :> ReqBody '[JSON] RentalRequest :> Post '[JSON] RentalResponse
-- Read
type GetRentalsByUser = "rentals" :> "user" :> Capture "user_id" (InternalId User) :> Get '[JSON] [Rental]
type GetRentalByCopy  = "rentals" :> "copy" :> Capture "copy_id" (InternalId Copy) :> Get '[JSON] Rental
-- Update
type CompleteRental   = "rentals" :> "complete" :> ReqBody '[JSON] CompleteRentalRequest :> Post '[JSON] CompleteRentalResponse

-- | Extra
type Docs  = "docs.md" :> Get '[PlainText] Text
type StaticFiles = "static" :> Raw
type Index = Get '[HTML] Text
