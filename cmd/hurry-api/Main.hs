module Main (main) where

import Relude

import Main.Utf8 (withUtf8)
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, setInstallShutdownHandler, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Options.Applicative.Builder (auto, fullDesc, help, info, long, metavar, option, progDesc, strOption)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types (Parser, ParserInfo)
import System.Posix (Handler (..), installHandler, sigTERM)

import Hurry.Server (initializeServer)

data Options = Options {cachePath :: FilePath, port :: Port}

optionsP :: Parser Options
optionsP =
  Options
    <$> strOption (long "hurry_cache_path" <> metavar "FILEPATH" <> help "Path to local folder for storing cached units")
    <*> option auto (long "hurry_port" <> metavar "PORT" <> help "Port that the Hurry API should listen on")

argparser :: ParserInfo Options
argparser = withInfo optionsP "A server for caching compiled units."

withInfo :: Parser a -> String -> ParserInfo a
withInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

main :: IO ()
main = withUtf8 $ do
  hSetBuffering stdout LineBuffering
  Options{cachePath, port} <- execParser argparser
  withStdoutLogger $ \logger -> do
    let
      settings =
        defaultSettings
          & setPort port
          & setLogger logger
          & setInstallShutdownHandler shutdownHandler
    putStrLn "Starting web server..."
    server <- initializeServer cachePath
    putStrLn $ "Now listening on port " <> show port
    runSettings settings server
 where
  shutdownHandler closeSocketAction =
    void $ installHandler sigTERM (CatchOnce closeSocketAction) Nothing
