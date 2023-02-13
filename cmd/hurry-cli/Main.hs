module Main (main) where

import Relude

import Cabal.Plan (PlanJson (..))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative.Builder (command, fullDesc, info, progDesc, subparser)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types (Parser, ParserInfo)

import Hurry.Lockfile (createLockfile)

data Subcommand = Lock | Save | Restore | Verify

subcommandP :: Parser Subcommand
subcommandP =
  subparser $
    command "lock" (withInfo (pure Lock) "Generate a lockfile")
      <> command "save" (withInfo (pure Save) "Cache dependencies specified by lockfile")
      <> command "restore" (withInfo (pure Restore) "Install dependencies from lockfile")
      <> command "verify" (withInfo (pure Verify) "Check whether lockfile needs to be updated")

argparser :: ParserInfo Subcommand
argparser = withInfo subcommandP "A build tool for Haskell applications that's fast and easy to use."

withInfo :: Parser a -> String -> ParserInfo a
withInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

main :: IO ()
main = do
  args <- execParser argparser
  case args of
    Lock -> lockCmd
    Save -> putStrLn "not yet implemented"
    Restore -> putStrLn "Not yet implemented"
    Verify -> putStrLn "Not yet implemented"

lockCmd :: IO ()
lockCmd = do
  -- Load the Cabal install plan.
  cabalPlanJSON <- eitherDecodeFileStrict @PlanJson "dist-newstyle/cache/plan.json"
  cabalPlan <- case cabalPlanJSON of
    Right p -> pure p
    Left err -> die $ "Could not load dist-newstyle/cache/plan.json: " <> err

  -- Create a lockfile.
  let lockfile = createLockfile cabalPlan

  -- Write the lockfile.
  writeFileBS "hurry.lock" $ toStrict $ encodePretty lockfile
