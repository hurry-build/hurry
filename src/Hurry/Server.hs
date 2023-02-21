module Hurry.Server (initializeServer) where

import Relude

import Control.Monad.Except (throwError)
import Data.ByteString qualified as BS
import Servant.API (Capture, Get, JSON, NoContent (..), OctetStream, Put, ReqBody, (:<|>) (..), (:>))
import Servant.API.Verbs (HeadNoContent)
import Servant.Server (Application, Handler, Server, ServerError (..), err404, serve)
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize)
import System.FilePath ((</>))

-- This iteration of the API is inspired by https://github.com/buchgr/bazel-remote/
--
-- Notice that this cache is by unit IDs for now, and in particular is not a
-- CAS.
type HurryAPI =
  "cache" :> Capture "unitId" Text :> HeadNoContent
    :<|> "cache" :> Capture "unitId" Text :> Get '[OctetStream] ByteString
    :<|> "cache" :> Capture "unitId" Text :> ReqBody '[OctetStream] ByteString :> Put '[JSON] ()

-- TODO: Make this cache path configurable.
-- TODO: Support pluggable object storage backends.
cachePath :: FilePath
cachePath = "/tmp/hurry-cache"

initializeServer :: IO Application
initializeServer = do
  -- Ensure that the cache folder has been created.
  createDirectoryIfMissing True cachePath

  -- Provide the application.
  pure hurryAPI

hurryAPI :: Application
hurryAPI = serve (Proxy @HurryAPI) server
 where
  server :: Server HurryAPI
  server =
    handleCheckExistence
      :<|> handleDownload
      :<|> handleUpload

  handleCheckExistence :: Text -> Handler NoContent
  handleCheckExistence unitId = do
    putStrLn "Got HEAD request"
    print unitId
    let cachedUnitFilePath = cachePath </> (toString unitId <> ".tar.zst")
    unitCached <- liftIO $ doesFileExist cachedUnitFilePath
    if unitCached
      then do
        contentLength <- liftIO $ getFileSize cachedUnitFilePath
        -- This is an ugly hack so I can return 200 for a HeadNoContent, to
        -- match how bazel-remote works.
        throwError
          ServerError
            { errHTTPCode = 200
            , errReasonPhrase = "OK"
            , errBody = ""
            , errHeaders = [("Content-Length", show contentLength)]
            }
      else throwError err404

  handleDownload :: Text -> Handler ByteString
  handleDownload unitId = do
    putStrLn "Got GET request"
    print unitId
    pure ""

  handleUpload :: Text -> ByteString -> Handler ()
  handleUpload unitId uploaded = do
    putStrLn "Got PUT request"
    print unitId
    print $ BS.length uploaded
    pure ()
