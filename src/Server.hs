{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Server where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Char8      as BSC
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Time
import           Data.UUID.V4
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude                    hiding (id)
import           Servant
import           Servant.Docs               hiding (API, notes)
import           Servant.Docs.Internal      hiding (API)
import System.IO

import           API
import           Config
import qualified Persistence                as P
import           Types


type Pancake = ReaderT ServerConfig (ExceptT ServantErr IO)

startApp :: IO ()
startApp = do
  conn <- initConn
  P.initStore conn
  writeDocs
  hPutStrLn stderr "Server now running"
  run 8080 (app $ ServerConfig conn)
  where
    initConn :: IO (P.Conn P.Postgres)
    initConn = do
      threadDelay 3000000
      catch (P.initConnection P.defaultPostgres) $ \(_ :: SomeException) -> do
        hPutStrLn stderr "Failed to connect to db, retrying"
        initConn


app :: ServerConfig -> Application
app conf = serve fullApi (server conf)

fullApi :: Proxy FullAPI
fullApi = Proxy

api :: Proxy API
api = Proxy

server :: ServerConfig -> Server FullAPI
server conf = (enter (runReaderTNat conf)
  (serveDocs :<|> redirectToLanding :<|>
    (addUser :<|> getUsers :<|> getUserById :<|> deleteUser) :<|>
    (addBook :<|> getBooks :<|> getBookByIsbn :<|> deleteBook) :<|>
    (addCopy :<|> getCopies :<|> getCopyById  :<|> updateCopy :<|> deleteCopy) :<|>
    (rentCopy :<|> completeRental :<|> getRentalsByUser :<|> getRentalByCopy)
  )) :<|> staticFiles

staticFiles :: Server Raw
staticFiles = serveDirectory "static"

serverDocs :: Text
serverDocs = T.pack $ markdown cleanedDocs
  where
    rawDocs = docsWith (DocOptions 3) [howToRun, howToBuild] mempty (pretty api)
    cleanedDocs = rawDocs & apiEndpoints %~ fmap (\act -> act & authInfo %~ [DocAuthentication "foo" "bar"])

howToRun :: DocIntro
howToRun = DocIntro "How to run" mempty

howToBuild :: DocIntro
howToBuild = DocIntro "How to build from source" mempty

serveDocs :: Pancake Text
serveDocs = return serverDocs

writeDocs :: IO ()
writeDocs = T.writeFile "README.md" serverDocs

--------------------------------------------------------------------------------

-- Users

addUser :: AddUserRequest -> Pancake AddUserResponse
addUser AddUserRequest{..} = do
  ServerConfig{..} <- ask
  uuid <- liftIO $ InternalId <$> nextRandom
  liftIO $ P.addUser serverStore (User name uuid)
  return $ AddUserResponse uuid

getUsers :: Maybe Name -> Pancake [User]
getUsers Nothing = do
  ServerConfig{..} <- ask
  liftIO $ P.getAllUsers serverStore
getUsers (Just (Name searchTerm)) = do
  ServerConfig{..} <- ask
  liftIO $ P.searchUsersByName serverStore searchTerm

getUserById :: InternalId User -> Pancake User
getUserById ident = do
  ServerConfig{..} <- ask
  maybeUser <- liftIO $ P.getUserById serverStore ident
  case maybeUser of
    Just user -> return user
    Nothing   -> throwError err404

deleteUser :: InternalId User -> Pancake NoContent
deleteUser ident = do
  ServerConfig{..} <- ask
  _ <- getUserById ident
  liftIO $ P.deleteUser serverStore ident
  return NoContent


--------------------------------------------------------------------------------

-- Books

addBook :: Book -> Pancake NoContent
addBook book = do
  ServerConfig{..} <- ask
  liftIO $ P.addBook serverStore book
  return NoContent

getBooks :: Maybe Name -> Pancake [Book]
getBooks Nothing = do
  ServerConfig{..} <- ask
  liftIO $ P.getAllBooks serverStore
getBooks (Just (Name searchTerm)) = do
  ServerConfig{..} <- ask
  liftIO $ P.searchBooksByTitle serverStore searchTerm


getBookByIsbn :: ISBN -> Pancake Book
getBookByIsbn isbn = do
  ServerConfig{..} <- ask
  maybeBook <- liftIO $ P.getBookByIsbn serverStore isbn
  case maybeBook of
    Just book -> return book
    Nothing   -> throwError err404

deleteBook :: ISBN -> Pancake NoContent
deleteBook isbn = do
  ServerConfig{..} <- ask
  _ <- getBookByIsbn isbn
  liftIO $ P.deleteBook serverStore isbn
  return NoContent

--------------------------------------------------------------------------------

-- Copies

addCopy :: ISBN -> AddCopyRequest -> Pancake AddCopyResponse
addCopy isbn acr = do
  ServerConfig{..} <- ask
  copy@Copy{..} <- liftIO $ acrToCopy isbn acr
  successful <- liftIO $ P.addCopy serverStore copy
  return $ if successful then
    AddCopyResponse (Just id) True
  else
    AddCopyResponse Nothing False

getCopies :: ISBN -> Pancake [Copy]
getCopies isbn = do
  ServerConfig{..} <- ask
  liftIO $ P.getCopiesByIsbn serverStore isbn

getCopyById :: InternalId Copy -> Pancake Copy
getCopyById ident = do
  ServerConfig{..} <- ask
  ret <- liftIO $ P.getCopyById serverStore ident
  case ret of
    Nothing -> do
      liftIO $ hPutStrLn stderr $ "Can't find copy uuid: " <> show ident
      throwError err404
    Just c  -> do
      liftIO $ hPutStrLn stderr $ "Successfully found copy with uuid " <> show ident
      return c

updateCopy :: InternalId Copy -> UpdateCopyRequest -> Pancake NoContent
updateCopy ident AddCopyRequest{..} = do
  ServerConfig{..} <- ask
  liftIO $ P.updateCopy serverStore ident notes
  return NoContent


deleteCopy :: InternalId Copy -> Pancake NoContent
deleteCopy ident = do
  ServerConfig{..} <- ask
  _ <- getCopyById ident
  liftIO $ P.deleteCopy serverStore ident
  return NoContent

--------------------------------------------------------------------------------

-- Rentals

rentCopy :: RentalRequest -> Pancake RentalResponse
rentCopy RentalRequest{..} = do
  ServerConfig{..} <- ask
  _ <- getCopyById copyId
  _ <- getUserById userId
  mRental <- liftIO $ P.getCurrentRentalByCopy serverStore copyId
  case mRental of
    Nothing -> do
      uuid <- liftIO $ InternalId <$> nextRandom
      liftIO $ P.addRental serverStore (Rental uuid copyId userId dueDate Nothing)
      return $ RentalResponse (Just uuid) True
    Just _ -> return $ RentalResponse Nothing False

getRentalsByUser :: InternalId User -> Pancake [Rental]
getRentalsByUser ident = do
  ServerConfig{..} <- ask
  _ <- getUserById ident
  liftIO $ P.getRentalsByUser serverStore ident

getRentalByCopy :: InternalId Copy -> Pancake Rental
getRentalByCopy ident = do
  ServerConfig{..} <- ask
  _ <- getCopyById ident
  mRental <- liftIO $ P.getCurrentRentalByCopy serverStore ident
  case mRental of
    Nothing     -> throwError err404
    Just rental -> return rental

completeRental :: CompleteRentalRequest -> Pancake CompleteRentalResponse
completeRental CompleteRentalRequest{..} = do
  ServerConfig{..} <- ask
  mRental <- liftIO $ P.getRental serverStore rentalId
  case mRental of
    Nothing -> return NoSuchRental
    Just Rental{..} -> case returnDate of
      Just _ -> return RentalAlreadyComplete
      Nothing -> do
        now <- liftIO $ getCurrentTime
        let nowString = formatTime defaultTimeLocale "%F" now
        let nowDate = parseTimeM True defaultTimeLocale "%F" nowString
        case nowDate of
          Nothing -> throwError err500
          Just d  -> do
            liftIO $ P.completeRental serverStore rentalId d
            return CompleteRentalSuccess

--------------------------------------------------------------------------------

-- Extra

redirectToDocs :: Pancake a
redirectToDocs = let redirectURI = safeLink fullApi (Proxy :: Proxy Docs)
                 in throwError $ err301{errHeaders=(hLocation, BSC.pack $ show redirectURI):errHeaders err301}

redirectToLanding :: Pancake a
redirectToLanding = let redirectURI = "/static/landing_page.html"
                    in throwError $ err301{errHeaders=(hLocation, redirectURI):errHeaders err301}

--------------------------------------------------------------------------------
