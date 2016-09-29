{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Control.Monad.Except
import qualified Data.ByteString.Char8     as BSC
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time
import qualified Data.Vector               as V
import           Data.UUID.V4
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Docs              hiding (API)

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
server = serveDocs :<|> index :<|> getAllUsers :<|> getAllBooks

serveDocs :: ExceptT ServantErr IO Text
serveDocs = return $ T.pack $ markdown $ docs $ pretty api

getAllUsers :: ExceptT ServantErr IO [User]
getAllUsers = do
  urAWizard <- liftIO nextRandom
  return [User "Harry Potter" (InternalId urAWizard)]

getAllBooks :: ExceptT ServantErr IO [Book]
getAllBooks = do
  now <- liftIO getCurrentTime
  return [Book "lol-legit-isbn" "A Story of Sadness" (V.fromList ["Emily Olorin", "Oswyn Brent"]) (V.fromList ["Sadness Publishing"]) now]

index :: ExceptT ServantErr IO a
index = let redirectURI = safeLink fullApi (Proxy :: Proxy Docs)
        in throwError $ err301{errHeaders=(hLocation, BSC.pack $ show redirectURI):errHeaders err301}
