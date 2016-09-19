{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Time
import           Data.UUID.V4
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server

import           API
import           Types

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getAllUsers :<|> getAllBooks

getAllUsers :: ExceptT ServantErr IO [User]
getAllUsers = do
  urAWizard <- liftIO nextRandom
  return [User "Harry Potter" (InternalId urAWizard)]

getAllBooks :: ExceptT ServantErr IO [Book]
getAllBooks = do
  now <- liftIO getCurrentTime
  return [Book "lol-legit-isbn" "A Story of Sadness" ["Emily Olorin", "Oswyn Brent"] ["Sadness Publishing"] now]
