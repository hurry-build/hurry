module Main (main) where

import Relude

import Cabal.Plan (PlanJson)
import Data.Aeson (eitherDecodeFileStrict)
import Options.Applicative.Builder (command, fullDesc, info, progDesc, subparser)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types (Parser, ParserInfo)
import Text.Pretty.Simple (pPrint)

data Subcommand = Lock | Restore | Verify | Doctor

subcommandP :: Parser Subcommand
subcommandP =
  subparser $
    command "lock" (withInfo (pure Lock) "Generate a lockfile based on the currently installed dependencies")
      <> command "restore" (withInfo (pure Restore) "Install dependencies from a lockfile")
      <> command "verify" (withInfo (pure Verify) "Check whether the current dependency install plan conflicts with a lockfile")
      <> command "doctor" (withInfo (pure Doctor) "Dump debug information")

argparser :: ParserInfo Subcommand
argparser = withInfo subcommandP "A build tool for Haskell applications that's fast and easy to use."

withInfo :: Parser a -> String -> ParserInfo a
withInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

main :: IO ()
main = do
  args <- execParser argparser
  case args of
    Lock -> lockCmd
    Restore -> putStrLn "Not yet implemented"
    Verify -> putStrLn "Not yet implemented"
    Doctor -> putStrLn "Time to print a bunch of stuff"

lockCmd :: IO ()
lockCmd = do
  -- Check whether cabal install plan exists.
  cabalPlanJSON <- eitherDecodeFileStrict @PlanJson "dist-newstyle/cache/plan.json"
  pPrint cabalPlanJSON
  -- If the install plan does exist, parse it.
  -- Write a lockfile that's restorable and cacheable
  putStrLn "Not yet implemented"

restoreCmd :: IO ()
restoreCmd = do
  -- TODO: How do I install a specific library package?
  undefined
