module Main (main) where

import Relude

import Cabal.Plan (PlanJson (..), UnitId (..), dispPkgId)
import Codec.Archive.Zip (ZipOption (..), addFilesToArchive, emptyArchive, fromArchive)
import Codec.Compression.Zstd qualified as Zstd
import Data.Aeson (decodeFileStrict', eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Network.HTTP.Req (HEAD (..), NoReqBody (NoReqBody), PUT (..), ReqBodyBs (ReqBodyBs), defaultHttpConfig, http, httpConfigCheckResponse, ignoreResponse, port, req, responseStatusCode, runReq, (/:))
import Options.Applicative.Builder (command, fullDesc, info, progDesc, subparser)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types (Parser, ParserInfo)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Pretty.Simple (pPrint)

import Hurry.Lockfile (Lockfile (..), createLockfile)

data Subcommand = Lock | Save | Restore | Verify | Doctor

subcommandP :: Parser Subcommand
subcommandP =
  subparser $
    command "lock" (withInfo (pure Lock) "Generate a lockfile")
      <> command "save" (withInfo (pure Save) "Cache dependencies specified by lockfile")
      <> command "restore" (withInfo (pure Restore) "Install dependencies from lockfile")
      <> command "verify" (withInfo (pure Verify) "Check whether lockfile needs to be updated")
      <> command "doctor" (withInfo (pure Doctor) "Dump debugging information")

argparser :: ParserInfo Subcommand
argparser = withInfo subcommandP "A build tool for Haskell applications that's fast and easy to use."

withInfo :: Parser a -> String -> ParserInfo a
withInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

main :: IO ()
main = do
  args <- execParser argparser
  case args of
    Lock -> lockCmd
    Save -> saveCmd
    Restore -> putStrLn "Not yet implemented"
    Verify -> putStrLn "Not yet implemented"
    Doctor -> doctorCmd

lockCmd :: IO ()
lockCmd = do
  -- Load the Cabal install plan.
  let planLocation = "dist-newstyle" </> "cache" </> "plan.json"
  cabalPlanJSON <- eitherDecodeFileStrict @PlanJson planLocation
  cabalPlan@PlanJson{pjUnits} <- case cabalPlanJSON of
    Right p -> pure p
    Left err -> die $ "Could not load " <> show planLocation <> ": " <> err

  -- Create a lockfile.
  putStrLn $ "Creating lockfile for " <> show (Map.size pjUnits) <> " dependencies..."
  let lockfile = createLockfile cabalPlan

  -- Write the lockfile.
  writeFileBS "hurry.lock" $ toStrict $ encodePretty lockfile
  putStrLn "Wrote lockfile to hurry.lock"

saveCmd :: IO ()
saveCmd = do
  -- TODO: Add authentication.

  -- Read the lockfile.
  lockfile <- decodeFileStrict' @Lockfile "hurry.lock"
  Lockfile{units, compiler} <- case lockfile of
    Just l -> pure l
    Nothing -> die "hurry.lock not found"
  putStrLn $ "Parsed " <> show (length units) <> " units"

  -- Compute Cabal store path.
  homeDir <- getHomeDirectory
  let storePath = homeDir </> ".cabal" </> "store" </> toString (dispPkgId compiler)

  -- For each unit, check if it has been cached already. If not, upload the
  -- unit.
  --
  -- TODO: Parallelize?
  for_ units $ \(UnitId unitId) -> do
    putTextLn $ "Caching unit " <> unitId
    -- Check if the unit has already been cached.
    headResponse <-
      runReq defaultHttpConfig{httpConfigCheckResponse = \_ _ _ -> Nothing} $
        req
          HEAD
          (http "localhost" /: "cache" /: unitId)
          NoReqBody
          ignoreResponse
          (port 8081)
    let headStatus = responseStatusCode headResponse
    case headStatus of
      -- Unit has already been cached. Skip.
      200 -> do
        putStrLn "Unit has already been cached"
        pass
      -- Unit has not yet been cached. Upload to cache.
      404 -> do
        putStrLn "Unit has not yet been cached"

        -- This is the zstd CLI's default compression level.
        let
          zstdCompressionLevel = 3

        -- Gather the unit's files into a single archive file and compress
        -- them.
        putStrLn $ "Gathering files from " <> show (storePath </> toString unitId)

        !payload <-
          Zstd.compress zstdCompressionLevel
            . toStrict
            . fromArchive
            <$> addFilesToArchive [OptRecursive] emptyArchive [storePath </> toString unitId]

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
      -- Unknown status code. Panic.
      status -> error $ "unknown response code: " <> show status

doctorCmd :: IO ()
doctorCmd = do
  -- Load the Cabal install plan.
  let planLocation = "dist-newstyle" </> "cache" </> "plan.json"
  cabalPlanJSON <- eitherDecodeFileStrict @PlanJson planLocation
  cabalPlan <- case cabalPlanJSON of
    Right p -> pure p
    Left err -> die $ "Could not load " <> show planLocation <> ": " <> err

  -- Pretty-print all the units for inspection.
  pPrint cabalPlan
