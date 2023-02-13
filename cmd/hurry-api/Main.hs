module Main (main) where

import Relude

import Network.Wai.Handler.Warp (run)

import Hurry.Server (hurryAPI)

main :: IO ()
main = do
  putStrLn "Now listening on port 8081"
  run 8081 hurryAPI
