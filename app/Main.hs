{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Map
import           Data.Maybe                  (fromMaybe)
import           Data.Text
import           Data.Text.Encoding
import           Data.Time
import           Hasql.Pool                  as P
import qualified Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Cors
import           Options.Generic
import           Servant

import           Lib

data CmdLine = CmdLine { _cmdLineConfigFile :: FilePath
                       } deriving Generic
instance ParseRecord CmdLine

data Layer = Layer { _layerQuery        :: String
                   , _layerLastModified :: UTCTime
                   } deriving (Show, Eq, Generic)

data Config = Config { _configPgConnection       :: Text
                     , _configMapnikInputPlugins :: Maybe FilePath
                     , _configPort               :: Maybe Int
                     , _configPgPoolSize         :: Maybe Int
                     , _configPgTimeout          :: Maybe NominalDiffTime
                     , _configLayers             :: Map Text Text
                     } deriving (Show, Generic)
instance FromJSON Config where

main :: IO ()
main = getRecord "hastile" >>= doIt

doIt :: CmdLine -> IO ()
doIt cmdLine = do
  startTime <- getCurrentTime
  configBs <- BSL.readFile $ _cmdLineConfigFile cmdLine
  case decode configBs of
    Just config -> doItWithConfig config startTime
    Nothing -> putStrLn $ "Failed to parse config file " ++ _cmdLineConfigFile cmdLine

doItWithConfig :: Config -> UTCTime -> IO ()
doItWithConfig config startTime =
  let startTime' = formatTime defaultTimeLocale rfc822DateFormat startTime
      startTime'' = Data.Text.concat [dropEnd 3 (pack startTime'), "GMT"]
      pgPoolSize' = fromMaybe 10 $ _configPgPoolSize config
      pgTimeout' = fromMaybe 1 $ _configPgTimeout config
      pluginDir' = fromMaybe "/usr/local/lib/mapnik/input" $ _configMapnikInputPlugins config
      port' = fromMaybe 8080 $ _configPort config
      layers' = _configLayers config
  in bracket (P.acquire (pgPoolSize', pgTimeout', encodeUtf8 $ _configPgConnection config))
          P.release $
     \p ->
       Warp.run port' . cors (const $ Just policy) . serve api $ hastileService (ServerState p pluginDir' (unpack startTime'') layers')
         where
           policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }
