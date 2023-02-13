module Hurry.Server (hurryAPI) where

import Relude

import Servant.API (Get, JSON, NoContent (..), Put, (:<|>) (..), (:>))
import Servant.API.Verbs (HeadNoContent)
import Servant.Server (Application, Server, serve)

type HurryAPI =
  "cache" :> Get '[JSON] ()
    :<|> "cache" :> Put '[JSON] ()
    :<|> "cache" :> HeadNoContent

hurryAPI :: Application
hurryAPI = serve (Proxy @HurryAPI) server
 where
  server :: Server HurryAPI
  server =
    pass
      :<|> pass
      :<|> pure NoContent
