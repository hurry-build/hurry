module Hurry.Client (Client (..), ClientM, runClient) where

import Relude

import Network.Wreq.Session (Session)
import Text.URI (URI)

data Client = Client
  { serverURL :: URI
  , session :: Session
  }

newtype ClientM a = ClientM {unClientM :: ReaderT Client IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Client)

runClient :: Client -> ClientM a -> IO a
runClient client = usingReaderT client . unClientM
