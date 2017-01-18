{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           Data.Time
import           Hasql.Pool                  as P
import           Network.Wai
import qualified Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Cors
import           Options.Generic
import           Servant
import           System.Exit

import           Lib
import           Types

main :: IO ()
main = getRecord "hastile" >>= doIt

doIt :: CmdLine -> IO ()
doIt cmdLine = do
  startTime <- getCurrentTime
  configBs <- BSL.readFile $ configFile cmdLine
  case eitherDecode configBs of
    Right config -> doItWithConfig config startTime
    Left e -> do
      putStrLn $ "In file: " <> configFile cmdLine <> "\nError: " <> e
      exitWith (ExitFailure 2)

doItWithConfig :: Config -> UTCTime -> IO ()
doItWithConfig config startTime =
  let startTime' = formatTime defaultTimeLocale rfc822DateFormat startTime
      startTime'' = dropEnd 3 (pack startTime') <> "GMT"
      pgPoolSize' = fromMaybe 10 $ _configPgPoolSize config
      pgTimeout' = fromMaybe 1 $ _configPgTimeout config
      pluginDir' = fromMaybe "/usr/local/lib/mapnik/input" $ _configMapnikInputPlugins config
      port' = fromMaybe 8080 $ _configPort config
      layers' = _configLayers config
  in bracket (P.acquire (pgPoolSize', pgTimeout', encodeUtf8 $ _configPgConnection config))
          P.release $
            \p -> getWarp port' . serve api $ hastileService (ServerState p pluginDir' (unpack startTime'') layers')

getWarp :: Warp.Port -> Network.Wai.Application -> IO ()
getWarp port' = Warp.run port' . cors (const $ Just policy)
           where
             policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }
