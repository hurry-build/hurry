module Hurry.Client.Cache (checkUnitAvailableLocally, checkUnitInCache, uploadUnitToCache, downloadUnitFromCache) where

import Relude
import Relude.Extra.Lens ((^.))

import Cabal.Plan (UnitId (..))
import Codec.Archive.Zip (ZipOption (..), addFilesToArchive, emptyArchive, extractFilesFromArchive, fromArchive, toArchive)
import Codec.Compression.Zstd (Decompress (..))
import Codec.Compression.Zstd qualified as Zstd
import Data.ByteString qualified as BS
import Hurry.Client.Lockfile (LockfileUnitInfo (LockfileUnitInfo, hasGhcPkgConf))
import Network.Wreq (defaults, responseBody, responseStatus, statusCode)
import Network.Wreq.Session qualified as Wreq
import Network.Wreq.Types (Options (..))
import System.Directory (doesDirectoryExist)
import System.FilePath ((<.>), (</>))
import Text.URI (URI (..), mkPathPiece, renderStr)
import Text.URI.QQ (pathPiece)

import Hurry.Client (Client (..))

checkUnitAvailableLocally :: MonadIO m => FilePath -> UnitId -> m Bool
checkUnitAvailableLocally storePath (UnitId unitId) = liftIO $ doesDirectoryExist $ storePath </> toString unitId

checkUnitInCache :: (MonadReader Client m, MonadIO m) => UnitId -> m Bool
checkUnitInCache (UnitId unitId) = do
  -- Check if the unit has already been cached.
  Client{session, serverURL} <- ask
  response <-
    liftIO
      $ Wreq.headWith
        defaults{checkResponse = Just $ \_ _ -> pass}
        session
      $ renderStr serverURL{uriPath = Just (False, [pathPiece|cache|] :| mkPathPiece unitId)}
  let headStatus = response ^. responseStatus . statusCode
  case headStatus of
    -- Unit has already been cached. Skip.
    200 -> pure True
    -- Unit has not yet been cached. Upload to cache.
    404 -> pure False
    -- Unknown status code. Panic.
    status -> error $ "unknown response code: " <> show status

uploadUnitToCache :: (MonadReader Client m, MonadIO m) => FilePath -> UnitId -> LockfileUnitInfo -> m ()
uploadUnitToCache storePath (UnitId unitId) LockfileUnitInfo{hasGhcPkgConf} = do
  -- Gather the unit's files into a single archive file and compress
  -- them.
  putStrLn $ "Gathering files from " <> show (storePath </> toString unitId)

  unitFiles <- liftIO $ addFilesToArchive [OptRecursive] emptyArchive [storePath </> toString unitId]
  unitAndConf <-
    if hasGhcPkgConf
      then liftIO $ addFilesToArchive [] unitFiles [storePath </> "package.db" </> toString unitId <.> "conf"]
      else pure unitFiles
  let !payload = Zstd.compress zstdCompressionLevel $ toStrict $ fromArchive unitAndConf

  putStrLn $ "Uploading unit to cache (size " <> show (BS.length payload) <> ")"

  -- Upload the compressed unit archive to the cache.
  Client{serverURL, session} <- ask
  void $
    liftIO $
      Wreq.put
        session
        (renderStr serverURL{uriPath = Just (False, [pathPiece|cache|] :| mkPathPiece unitId)})
        payload
 where
  -- This is the zstd CLI's default compression level.
  zstdCompressionLevel = 3

downloadUnitFromCache :: (MonadReader Client m, MonadIO m) => FilePath -> UnitId -> m ()
downloadUnitFromCache storePath (UnitId unitId) = do
  Client{serverURL, session} <- ask
  response <-
    liftIO $
      Wreq.get session $
        renderStr serverURL{uriPath = Just (False, [pathPiece|cache|] :| mkPathPiece unitId)}
  case Zstd.decompress $ toStrict $ response ^. responseBody of
    Decompress payload -> liftIO $ extractFilesFromArchive [OptDestination storePath] $ toArchive $ toLazy payload
    Error err -> error $ "downloadUnitFromCache: " <> toText err
    Skip -> error "downloadUnitFromCache: impossible: unit was compressed in streaming mode"
