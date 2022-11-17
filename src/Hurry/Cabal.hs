module Hurry.Cabal (loadPackageHash) where

import Relude

import Cabal.Plan (UnitId (..), PkgId, dispPkgId)
import System.Directory (getHomeDirectory)

loadPackageHash :: PkgId -> UnitId -> IO ByteString
loadPackageHash compilerID (UnitId unitID) = do
  home <- getHomeDirectory
  let packageHashLoc = home <> "/.cabal/store/" <> toString (dispPkgId compilerID) <> "/" <> toString unitID <> "/cabal-hash.txt"
  readFileBS packageHashLoc
