{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base
import           Data.Aeson
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Map
import           Data.Maybe                  (fromMaybe)
import           Data.Text
import           Data.Time
import           Hasql.Pool                  as P
import qualified Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Cors
import           Options.Generic
import           Servant

import           Lib

data CmdLine = CmdLine { configFile :: FilePath
                       }
               deriving Generic
instance ParseRecord CmdLine

data Config = Config { pgConnection       :: String
                     , mapnikInputPlugins :: Maybe FilePath
                     , port               :: Maybe Int
                     , pgPoolSize         :: Maybe Int
                     , pgTimeout          :: Maybe NominalDiffTime
                     , layers             :: Map Text Text
                     } deriving (Show, Generic)
instance FromJSON Config where

main :: IO ()
main = getRecord "hastile" >>= doIt

doIt :: CmdLine -> IO ()
doIt cmdLine = do
  startTime <- getCurrentTime
  configBs <- BSL.readFile $ configFile cmdLine
  case decode configBs of
    Just config -> doItWithConfig config startTime
    Nothing -> putStrLn $ "Failed to parse config file " ++ configFile cmdLine

doItWithConfig :: Config -> UTCTime -> IO ()
doItWithConfig config startTime =
  let startTime' = formatTime defaultTimeLocale rfc822DateFormat startTime
      startTime'' = Data.Text.concat [dropEnd 3 (pack startTime'), "GMT"]
      pgPoolSize' = fromMaybe 10 $ pgPoolSize config
      pgTimeout'  = fromMaybe 1 $ pgTimeout config
      pluginDir'   = fromMaybe "/usr/local/lib/mapnik/input" $ mapnikInputPlugins config
      port'       = fromMaybe 8080 $ port config
      layers'    = layers config
  in bracket (P.acquire (pgPoolSize', pgTimeout', BS.pack . pgConnection $ config))
          P.release $
     \p ->
       Warp.run port' . cors (const $ Just policy) . serve api $ hastileService (ServerState p pluginDir' (unpack startTime'') layers')
         where
           policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }
