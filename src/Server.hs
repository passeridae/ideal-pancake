{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                (Text)
import           Data.Time
import           Data.UUID.V4
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Docs             hiding (API)
import           Servant.Server
import qualified Data.Text as T

import           API
import           Types

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve fullApi server

fullApi :: Proxy FullAPI
fullApi = Proxy

api :: Proxy API
api = Proxy

server :: Server FullAPI
server = serveDocs :<|> getAllUsers :<|> getAllBooks

serveDocs :: ExceptT ServantErr IO Text
serveDocs = do
  let realDocs = markdown $ docs api
  return $ T.pack realDocs

getAllUsers :: ExceptT ServantErr IO [User]
getAllUsers = do
  urAWizard <- liftIO nextRandom
  return [User "Harry Potter" (InternalId urAWizard)]

getAllBooks :: ExceptT ServantErr IO [Book]
getAllBooks = do
  now <- liftIO getCurrentTime
  return [Book "lol-legit-isbn" "A Story of Sadness" ["Emily Olorin", "Oswyn Brent"] ["Sadness Publishing"] now]
