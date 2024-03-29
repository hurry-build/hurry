module Main (main) where

import Relude

import Cabal.Plan (PlanJson (..), dispPkgId)
import Data.Aeson (decodeFileStrict', eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map.Strict qualified as Map
import Network.Wreq.Session (newSession)
import Options.Applicative.Builder (command, fullDesc, help, info, long, maybeReader, metavar, option, progDesc, subparser)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types (Parser, ParserInfo)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Pretty.Simple (pPrint)
import Text.URI (URI, mkURI)

import Hurry.Client (Client (..), ClientM, runClient)
import Hurry.Client.Cache (checkUnitAvailableLocally, checkUnitInCache, downloadUnitFromCache, uploadUnitToCache)
import Hurry.Client.Lockfile (Lockfile (..), createLockfile)

data Subcommand
  = Lock
  | Save {serverURL :: URI}
  | Restore {serverURL :: URI}
  | Verify
  | Doctor

subcommandP :: Parser Subcommand
subcommandP =
  subparser $
    command "lock" (withInfo (pure Lock) "Generate a lockfile")
      <> command "save" (withInfo saveP "Cache dependencies specified by lockfile")
      <> command "restore" (withInfo restoreP "Install dependencies from lockfile")
      <> command "verify" (withInfo (pure Verify) "Check whether lockfile needs to be updated")
      <> command "doctor" (withInfo (pure Doctor) "Dump debugging information")
 where
  serverP :: Parser URI
  serverP =
    option
      (maybeReader $ \s -> mkURI (toText s))
      (long "server" <> metavar "URL" <> help "Hurry API server URL")

  saveP :: Parser Subcommand
  saveP = Save <$> serverP

  restoreP :: Parser Subcommand
  restoreP = Restore <$> serverP

argparser :: ParserInfo Subcommand
argparser = withInfo subcommandP "A build tool for Haskell applications that's fast and easy to use."

withInfo :: Parser a -> String -> ParserInfo a
withInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

main :: IO ()
main = do
  args <- execParser argparser
  case args of
    Lock -> lockCmd
    Save{serverURL} -> clientCmd serverURL saveCmd
    Restore{serverURL} -> clientCmd serverURL restoreCmd
    Verify -> putStrLn "Not yet implemented"
    Doctor -> doctorCmd
 where
  clientCmd serverURL cmd = do
    session <- newSession
    runClient Client{serverURL, session} cmd

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

saveCmd :: ClientM ()
saveCmd = do
  -- TODO: Add authentication.

  -- Read the lockfile.
  lockfile <- liftIO $ decodeFileStrict' @Lockfile "hurry.lock"
  Lockfile{units, compiler} <- case lockfile of
    Just l -> pure l
    Nothing -> die "hurry.lock not found"
  putStrLn $ "Parsed " <> show (length units) <> " units"

  -- Compute Cabal store path.
  homeDir <- liftIO getHomeDirectory
  let storePath = homeDir </> ".cabal" </> "store" </> toString (dispPkgId compiler)

  -- For each unit, check if it has been cached already. If not, upload the
  -- unit.
  --
  -- TODO: Parallelize?
  for_ (Map.toList units) $ \(unitId, unitInfo) -> do
    putTextLn $ "Caching unit " <> show unitId
    -- Check if the unit has already been cached.
    cached <- checkUnitInCache unitId
    if cached
      then do
        -- Unit has already been cached. Skip.
        putStrLn "Unit has already been cached"
        pass
      else do
        -- Unit has not yet been cached. Upload to cache.
        putStrLn "Unit has not yet been cached"
        uploadUnitToCache storePath unitId unitInfo

restoreCmd :: ClientM ()
restoreCmd = do
  -- Read the lockfile.
  lockfile <- liftIO $ decodeFileStrict' @Lockfile "hurry.lock"
  Lockfile{units, compiler} <- case lockfile of
    Just l -> pure l
    Nothing -> die "hurry.lock not found"
  putStrLn $ "Parsed " <> show (length units) <> " units"

  -- Check which compiled units are missing.
  homeDir <- liftIO getHomeDirectory
  let storePath = homeDir </> ".cabal" </> "store" </> toString (dispPkgId compiler)

  for_ (Map.toList units) $ \(unitId, _) -> do
    -- Check if the unit exists.
    --
    -- TODO: We should probably also check whether the compiled unit folder's
    -- contents are valid via checksum.
    putStrLn $ "Checking whether to restore unit " <> show unitId
    unitBuilt <- checkUnitAvailableLocally storePath unitId
    if unitBuilt
      then putStrLn "Unit is already built locally"
      else do
        -- Restore unit from cache if available.
        putStrLn "Restoring unit from cache"
        unitCached <- checkUnitInCache unitId
        if unitCached
          then downloadUnitFromCache storePath unitId >> putStrLn "Unit restored from cache"
          else putStrLn $ "Warning: unit " <> show unitId <> " is neither available nor cached"

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
