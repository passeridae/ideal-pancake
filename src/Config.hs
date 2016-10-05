module Config where

import           Persistence

data ServerConfig = ServerConfig
  { serverStore :: Conn InMemory
  }
