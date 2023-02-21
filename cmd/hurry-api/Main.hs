module Main (main) where

import Relude

import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

import Hurry.Server (initializeServer)

main :: IO ()
main = do
  withStdoutLogger $ \logger -> do
    let
      port = 8081
      settings =
        defaultSettings
          & setPort port
          & setLogger logger
    putStrLn "Starting web server..."
    server <- initializeServer
    putStrLn $ "Now listening on port " <> show port
    runSettings settings server
