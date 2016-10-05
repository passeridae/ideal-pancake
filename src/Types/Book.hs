{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types.Book where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.String                          hiding (fromString)
import           Data.Text                            (Text)
import           Data.Time
import           Data.Vector                          (Vector)
import qualified Data.Vector                          as V
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Servant
import           Servant.Docs

--------------------------------------------------------------------------------

-- clarity type aliases

type Author = Text
type Publisher = Text
type Title = Text

--------------------------------------------------------------------------------

-- ISBN

newtype ISBN = ISBN
  { unISBN :: Text
  } deriving (Generic, Show, Ord, Eq, FromHttpApiData, FromJSON, ToJSON, IsString, FromField, ToField)

instance ToSample ISBN where
  toSamples _ = samples $ map ISBN ["9780060567231", "9780374529529"]

instance ToCapture (Capture "book_isbn" ISBN) where
  toCapture _ = DocCapture "book_isbn" "isbn of the book"

--------------------------------------------------------------------------------

-- Book

data Book = Book
  { isbn              :: ISBN
  , title             :: Title
  , authors           :: Vector Author
  , publishers        :: Vector Publisher
  , yearOfPublication :: Day
  } deriving (Generic, Show, Ord, Eq)

instance ToJSON Book where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

instance FromJSON Book where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

instance ToSample Book where
  toSamples _ = singleSample $ Book "lol-legit-isbn" "A Story of Sadness" (V.fromList ["Emily Olorin", "Oswyn Brent"]) (V.fromList ["Sadness Publishing"]) (fromGregorian 2016 09 30)

instance FromRow Book where
  fromRow = Book <$> field <*> field <*> field <*> field <*> field

instance ToRow Book where
  toRow b = toRow (isbn b,title b, authors b, publishers b, yearOfPublication b)

--------------------------------------------------------------------------------
