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

import           API
import           Config
import           Html
import qualified Persistence               as P
import           Text.Blaze.Html5
import           Types


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
  (serveDocs :<|> index :<|>
    (addUser :<|> getUsers :<|> getUserById :<|> updateUser :<|> deleteUser) :<|>
    (addBook :<|> getAllBooks :<|> getBookByIsbn) :<|>
    (addCopy :<|> getCopies :<|> updateCopy :<|> deleteCopy) :<|>
    (rentCopy :<|> completeRental)
  )

staticFiles :: Server Raw
staticFiles = serveDirectory "static"

serveDocs :: Pancake Text
serveDocs = return $ T.pack $ markdown $ docsWithOptions (pretty api) (DocOptions 2)

index :: Pancake Html
index = return indexHtml

--------------------------------------------------------------------------------

-- Users

addUser :: AddUserRequest -> Pancake AddUserResponse
addUser AddUserRequest{..} = do
  ServerConfig{..} <- ask
  uuid <- liftIO $ InternalId <$> nextRandom
  liftIO $ atomically $ P.addUser serverStore (User name uuid)
  return $ AddUserResponse uuid

getUsers :: Maybe Name -> Pancake [User]
getUsers Nothing = do
  ServerConfig{..} <- ask
  liftIO $ atomically $ P.getAllUsers serverStore
getUsers (Just (Name searchTerm)) = do
  ServerConfig{..} <- ask
  liftIO $ atomically $ P.getUsersByName serverStore searchTerm

getUserById :: InternalId User -> Pancake User
getUserById ident = do
  ServerConfig{..} <- ask
  maybeUser <- liftIO $ atomically $ P.getUserById serverStore ident
  case maybeUser of
    Just user -> return user
    Nothing   -> throwError err404

updateUser :: InternalId User -> UpdateUserRequest -> Pancake UpdateUserResponse
updateUser = error "NYI"

deleteUser :: InternalId User -> Pancake NoContent
deleteUser = error "NYI"

--------------------------------------------------------------------------------

-- Books

addBook :: Book -> Pancake NoContent
addBook book = do
  ServerConfig{..} <- ask
  liftIO $ atomically $ P.addBook serverStore book
  return NoContent

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

--------------------------------------------------------------------------------

-- Copies

addCopy :: ISBN -> AddCopyRequest -> Pancake AddCopyResponse
addCopy isbn acr = do
  ServerConfig{..} <- ask
  copy@Copy{..} <- liftIO $ acrToCopy isbn acr
  successful <- liftIO $ atomically $ P.addCopy serverStore copy
  return $ if successful then
    AddCopyResponse (Just id) True
  else
    AddCopyResponse Nothing False

getCopies :: ISBN -> Pancake [Copy]
getCopies = error "NYI"

updateCopy :: InternalId Copy -> UpdateCopyRequest -> Pancake NoContent
updateCopy = error "NYI"

deleteCopy :: InternalId Copy -> Pancake NoContent
deleteCopy = error "NYI"

--------------------------------------------------------------------------------

-- Rentals

rentCopy :: RentalRequest -> Pancake RentalResponse
rentCopy = error "NYI"

completeRental :: CompleteRentalRequest -> Pancake CompleteRentalResponse
completeRental = error "NYI"

--------------------------------------------------------------------------------

-- Extra

redirectToDocs :: Pancake a
redirectToDocs = let redirectURI = safeLink fullApi (Proxy :: Proxy Docs)
                 in throwError $ err301{errHeaders=(hLocation, BSC.pack $ show redirectURI):errHeaders err301}

--------------------------------------------------------------------------------
