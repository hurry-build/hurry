module Hurry.Lockfile () where

import Relude

import Data.Aeson (FromJSON, ToJSON)

-- TODO: Add versioning before this becomes publicly accessible.
data Lockfile = Lockfile {

} deriving (Generic, ToJSON, FromJSON)
