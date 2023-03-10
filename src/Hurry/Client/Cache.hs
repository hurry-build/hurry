module Hurry.Client.Cache (checkUnitAvailableLocally, checkUnitInCache, uploadUnitToCache, downloadUnitFromCache) where

import Relude

import Cabal.Plan (UnitId (..))
import Codec.Archive.Zip (ZipOption (..), addFilesToArchive, emptyArchive, extractFilesFromArchive, fromArchive, toArchive)
import Codec.Compression.Zstd (Decompress (..))
import Codec.Compression.Zstd qualified as Zstd
import Data.ByteString qualified as BS
import Hurry.Client.Lockfile (LockfileUnitInfo (LockfileUnitInfo, hasGhcPkgConf))
import Network.HTTP.Req (GET (..), HEAD (..), NoReqBody (NoReqBody), PUT (..), ReqBodyBs (ReqBodyBs), bsResponse, defaultHttpConfig, http, httpConfigCheckResponse, ignoreResponse, port, req, responseBody, responseStatusCode, runReq, (/:))
import System.Directory (doesDirectoryExist)
import System.FilePath ((<.>), (</>))

checkUnitAvailableLocally :: FilePath -> UnitId -> IO Bool
checkUnitAvailableLocally storePath (UnitId unitId) = doesDirectoryExist $ storePath </> toString unitId

checkUnitInCache :: UnitId -> IO Bool
checkUnitInCache (UnitId unitId) = do
  -- Check if the unit has already been cached.
  response <-
    runReq defaultHttpConfig{httpConfigCheckResponse = \_ _ _ -> Nothing} $
      req
        HEAD
        (http "localhost" /: "cache" /: unitId)
        NoReqBody
        ignoreResponse
        (port 8081)
  let headStatus = responseStatusCode response
  case headStatus of
    -- Unit has already been cached. Skip.
    200 -> pure True
    -- Unit has not yet been cached. Upload to cache.
    404 -> pure False
    -- Unknown status code. Panic.
    status -> error $ "unknown response code: " <> show status

uploadUnitToCache :: FilePath -> UnitId -> LockfileUnitInfo -> IO ()
uploadUnitToCache storePath (UnitId unitId) LockfileUnitInfo{hasGhcPkgConf} = do
  -- Gather the unit's files into a single archive file and compress
  -- them.
  putStrLn $ "Gathering files from " <> show (storePath </> toString unitId)

  unitFiles <- addFilesToArchive [OptRecursive] emptyArchive [storePath </> toString unitId]
  unitAndConf <-
    if hasGhcPkgConf
      then addFilesToArchive [] unitFiles [storePath </> "package.db" </> toString unitId <.> "conf"]
      else pure unitFiles
  let !payload = Zstd.compress zstdCompressionLevel $ toStrict $ fromArchive unitAndConf

  putStrLn $ "Uploading unit to cache (size " <> show (BS.length payload) <> ")"

  -- Upload the compressed unit archive to the cache.
  void $
    runReq defaultHttpConfig $
      req
        PUT
        (http "localhost" /: "cache" /: unitId)
        (ReqBodyBs payload)
        ignoreResponse
        (port 8081)
 where
  -- This is the zstd CLI's default compression level.
  zstdCompressionLevel = 3

downloadUnitFromCache :: FilePath -> UnitId -> IO ()
downloadUnitFromCache storePath (UnitId unitId) = do
  response <-
    runReq defaultHttpConfig $
      req
        GET
        (http "localhost" /: "cache" /: unitId)
        NoReqBody
        bsResponse
        (port 8081)
  case Zstd.decompress $ responseBody response of
    Decompress payload -> extractFilesFromArchive [OptDestination storePath] $ toArchive $ toLazy payload
    Error err -> error $ "downloadUnitFromCache: " <> toText err
    Skip -> error "downloadUnitFromCache: impossible: unit was compressed in streaming mode"
