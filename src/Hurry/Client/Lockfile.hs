module Hurry.Client.Lockfile (Lockfile (..), createLockfile) where

import Relude

import Algebra.Graph (Graph, edge, overlays, simplify)
import Algebra.Graph.ToGraph (reachable)
import Cabal.Plan (CompInfo (..), PkgId, PlanJson (..), Unit (..), UnitId (..), UnitType (..))
import Data.Aeson (Encoding, FromJSON, ToJSON (..), defaultOptions, genericToEncoding)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

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
    { -- Construct a graph of units, keeping track of local units. After
      -- construction, prune any vertices that are unreachable from local units.
      --
      -- We do the extra effort of constructing and pruning the graph because
      -- the units list in the Cabal plan contains every component of every
      -- external package. Some of these components might be unused (e.g. if a
      -- package is depended on for its library component, but also contains an
      -- unused executable component that provides a command-line tool), and
      -- components that are unused will not be present in the Cabal store
      -- (because they never got built).
      --
      -- Unit IDs are sorted so that installation plans with the same units are
      -- stable.
      units = sort $ toList reachableLockableUnits
    , compiler = pjCompilerId
    }
 where
  unitToGraph :: Unit -> Graph UnitId
  unitToGraph Unit{uId, uComps} =
    overlays
      $ concatMap
        ( \CompInfo{ciLibDeps, ciExeDeps} ->
            edge uId <$> (toList ciLibDeps <> toList ciExeDeps)
        )
      $ toList uComps

  allUnits :: Graph UnitId
  allUnits = simplify $ overlays $ unitToGraph <$> toList pjUnits

  localUnits :: [Unit]
  localUnits = filter (\Unit{uType} -> uType == UnitTypeLocal) $ toList pjUnits

  reachableUnits :: Set UnitId
  reachableUnits = fromList $ concatMap (\Unit{uId} -> reachable allUnits uId) localUnits

  reachableLockableUnits :: Set UnitId
  reachableLockableUnits = Set.filter isLockable reachableUnits

  isLockable :: UnitId -> Bool
  isLockable unitID =
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
      UnitTypeBuiltin -> False
      -- Local units are the local packages currently being developed in the
      -- project. I want to eventually cache these, but the current version only
      -- caches external packages.
      --
      -- TODO: Add local package caching.
      UnitTypeLocal -> False
      -- I'm not totally sure what inplace units are. I think these are the same
      -- as local? Maybe these are external packages that depend upon local
      -- packages?
      UnitTypeInplace -> False
      -- Global units are the external packages stored in the global Cabal
      -- store, usually at `~/.cabal/store`.
      UnitTypeGlobal -> True
   where
    Unit{uType} = fromMaybe (error "impossible: unknown unit ID") $ Map.lookup unitID pjUnits
