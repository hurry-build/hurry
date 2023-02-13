module Hurry.Cabal (loadPackageHash) where

import Relude

import Cabal.Plan (PkgId, UnitId (..), dispPkgId)
import Control.Exception.Safe (IOException, catch)
import System.Directory (getHomeDirectory)

loadPackageHash :: PkgId -> UnitId -> IO (Maybe ByteString)
loadPackageHash compilerID (UnitId unitID) = do
  home <- getHomeDirectory
  let packageHashLoc = home <> "/.cabal/store/" <> toString (dispPkgId compilerID) <> "/" <> toString unitID <> "/cabal-hash.txt"
  (Just <$> readFileBS packageHashLoc) `catch` (\(_ :: IOException) -> pure Nothing)
