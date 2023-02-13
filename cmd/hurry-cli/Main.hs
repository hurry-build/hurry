module Main (main) where

import Relude

import Cabal.Plan (CompName (..), PlanJson (..), Unit (..), UnitType (..))
import Data.Aeson (eitherDecodeFileStrict)
import Options.Applicative.Builder (command, fullDesc, info, progDesc, subparser)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types (Parser, ParserInfo)
import Text.Pretty.Simple (pPrint)

import Data.Map.Strict qualified as Map
import Hurry.Cabal (loadPackageHash)

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
    Doctor -> doctorCmd

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

doctorCmd :: IO ()
doctorCmd = do
  cabalPlanJSONRaw <- eitherDecodeFileStrict @PlanJson "dist-newstyle/cache/plan.json"
  pPrint cabalPlanJSONRaw
  PlanJson{pjCompilerId, pjUnits} <- case cabalPlanJSONRaw of
    Right p -> pure p
    Left err -> die $ "Could not load dist-newstyle/cache/plan.json: " <> err
  for_ pjUnits $ \unit@Unit{uId, uType, uComps} -> do
    pPrint unit
    case uType of
      UnitTypeGlobal -> do
        let printHash = do
              packageHash <- loadPackageHash pjCompilerId uId
              pPrint packageHash
        -- TODO: libraries that are used only by exe components are not in ~/.cabal/store
        -- Need to prune exe's from components graph, then prune libs that are no longer reachable
        -- Alternatively, start from targets to build and then only look for reachable
        --
        -- Perhaps related to selectPlanSubset: https://github.com/haskell/cabal/blob/0837140e554f400106245f6cdf4d13cb7356a818/cabal-install/src/Distribution/Client/ProjectOrchestration.hs#L352-L355
        --
        -- Note that plan.json is written _before_ subset selection during `rebuildInstallPlan`:
        -- https://github.com/haskell/cabal/blob/master/cabal-install/src/Distribution/Client/ProjectPlanOutput.hs#L84
        -- https://github.com/haskell/cabal/blob/1d4491fb3dbad3deb55cbe8922e448d0cd467ba3/cabal-install/src/Distribution/Client/ProjectPlanning.hs#L720
        -- https://github.com/haskell/cabal/blob/master/cabal-install/src/Distribution/Client/ProjectOrchestration.hs#L352-L355
        for_ (Map.toList uComps) $ \(name, _) -> do
          case name of
            CompNameLib -> printHash
            CompNameSubLib _ -> printHash
            CompNameFLib _ -> printHash
            CompNameExe _ -> pass
            CompNameTest _ -> die $ "Unknown: component type: " <> show name
            CompNameBench _ -> die $ "Unknown: component type: " <> show name
            CompNameSetup -> printHash
      UnitTypeBuiltin -> pass
      UnitTypeLocal -> pass
      UnitTypeInplace -> pass
