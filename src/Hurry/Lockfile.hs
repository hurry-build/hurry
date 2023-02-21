module Hurry.Lockfile (Lockfile (..), createLockfile) where

import Relude

import Cabal.Plan (CompName (CompNameExe), PkgId, PlanJson (..), Unit (..), UnitId (..), UnitType (..))
import Data.Aeson (Encoding, FromJSON, ToJSON (..), defaultOptions, genericToEncoding)
import Data.Map.Strict qualified as Map

-- TODO: Add versioning before this becomes publicly accessible. The tool should
-- always support the current lockfile version and the immediately previous
-- lockfile version, and support upgrading from the immediately previous version
-- where possible.
data Lockfile = Lockfile
  { units :: [UnitId]
  , compiler :: PkgId
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

instance ToJSON Lockfile where
  toEncoding :: Lockfile -> Encoding
  toEncoding = genericToEncoding defaultOptions

createLockfile :: PlanJson -> Lockfile
createLockfile PlanJson{pjUnits, pjCompilerId} =
  Lockfile
    { -- Units are sorted so that installation plans with the same units are
      -- stable.
      units = sort $ mapMaybe (unitToString . snd) (Map.toList pjUnits)
    , compiler = pjCompilerId
    }
 where
  unitToString :: Unit -> Maybe UnitId
  unitToString Unit{uId, uType, uComps} =
    -- For the definition of "unit type", see: https://hackage.haskell.org/package/cabal-plan-0.7.2.3/docs/Cabal-Plan.html#t:UnitType
    -- For an explanation of package types, see: https://cabal.readthedocs.io/en/stable/nix-local-build.html#local-versus-external-packages
    case uType of
      -- Builtin units are those provided by the boot library packages of GHC
      -- itself. We don't need to lock builtin units, because those are not
      -- stored in `~/.cabal/store` and it would not make sense to save or
      -- restore them. They will by provided if the user is using the correct
      -- version of GHC, and will be unavailable otherwise.
      --
      -- For a list, see: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history
      UnitTypeBuiltin -> Nothing
      -- Local units are the local packages currently being developed in the
      -- project. I want to eventually cache these, but the current version only
      -- caches external packages.
      --
      -- TODO: Add local package caching.
      UnitTypeLocal -> Nothing
      -- I'm not totally sure what inplace units are. I think these are the same
      -- as local? Maybe these are external packages that depend upon local
      -- packages?
      UnitTypeInplace -> Nothing
      -- Global units are the external packages stored in the global Cabal
      -- store, usually at `~/.cabal/store`.
      UnitTypeGlobal -> case Map.toList uComps of
        -- Note that global units contain every component of every external
        -- package. Some of these components might be unused (e.g. if a package is
        -- depended on for its library component, but also contains an unused
        -- executable component that provides a command-line tool).
        --
        -- TODO: Is it possible that an Exe component here is actually used and
        -- should be cached? For example, could it be used by
        -- `build-tool-depends`?
        [(CompNameExe _, _)] -> Nothing
        _ -> Just uId
