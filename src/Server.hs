{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Server where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Char8     as BSC
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.UUID.V4
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude                   hiding (id)
import           Servant
import           Servant.Docs              hiding (API, notes)
import           Servant.HTML.Blaze

import           API
import           Config
import qualified Persistence               as P
import           Types
import           Html
import           Text.Blaze.Html5
import qualified Data.Vector as V
import           Data.Time.Calendar


type Pancake = ReaderT ServerConfig (ExceptT ServantErr IO)

startApp :: IO ()
startApp = do
  conn <- atomically $ P.initConnection P.X
  run 8080 (app $ ServerConfig conn)

app :: ServerConfig -> Application
app conf = serve fullApi (server conf)

fullApi :: Proxy FullAPI
fullApi = Proxy

api :: Proxy API
api = Proxy

server :: ServerConfig -> Server FullAPI
server conf = staticFiles :<|> enter (runReaderTNat conf)
  (serveDocs :<|> index :<|> getAllUsers :<|> getUserById :<|> addUser
    :<|> getAllBooks :<|> getBookByIsbn :<|> addBook
    :<|> addCopy)

serveDocs :: Pancake Text
serveDocs = return $ T.pack $ markdown $ docsWithOptions (pretty api) (DocOptions 2)

staticFiles :: Server Raw
staticFiles = serveDirectory "static"

getAllUsers :: Pancake [User]
getAllUsers = do
  ServerConfig{..} <- ask
  liftIO $ atomically $ P.getAllUsers serverStore

getUserById :: InternalId User -> Pancake User
getUserById ident = do
  ServerConfig{..} <- ask
  maybeUser <- liftIO $ atomically $ P.getUserById serverStore ident
  case maybeUser of
    Just user -> return user
    Nothing   -> throwError err404

addUser :: AddUserRequest -> Pancake AddUserResponse
addUser AddUserRequest{..} = do
  ServerConfig{..} <- ask
  uuid <- liftIO $ InternalId <$> nextRandom
  liftIO $ atomically $ P.addUser serverStore (User name uuid)
  return $ AddUserResponse uuid

getAllBooks :: Pancake [Book]
getAllBooks = do
  ServerConfig{..} <- ask
  liftIO $ atomically $ P.getAllBooks serverStore

getBookByIsbn :: ISBN -> Pancake Book
getBookByIsbn isbn = do
  ServerConfig{..} <- ask
  maybeBook <- liftIO $ atomically $ P.getBookByIsbn serverStore isbn
  case maybeBook of
    Just book -> return book
    Nothing   -> throwError err404

addBook :: Book -> Pancake NoContent
addBook book = do
  ServerConfig{..} <- ask
  liftIO $ atomically $ P.addBook serverStore book
  return NoContent

addCopy :: ISBN -> AddCopyRequest -> Pancake AddCopyResponse
addCopy isbn acr = do
  ServerConfig{..} <- ask
  copy@Copy{..} <- liftIO $ acrToCopy isbn acr
  successful <- liftIO $ atomically $ P.addCopy serverStore copy
  return $ if successful then
    AddCopyResponse (Just id) True
  else
    AddCopyResponse Nothing False

index :: Pancake Html
index =
  do books <- getAllBooks
     return . booksHtml $ books

-- redirect = let redirectURI = safeLink fullApi (Proxy :: Proxy Docs)
--            in throwError $
--               err301{errHeaders=(hLocation, BSC.pack $ show redirectURI):errHeaders err301}

