{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Control.Exception.Base            as ControlException
import qualified Data.Foldable                     as Foldable
import qualified Data.LruCache.IO                  as LRU
import qualified Data.Map                          as Map
import qualified Data.Text.Encoding                as TextEncoding
import           GHC.Conc
import qualified Hasql.Pool                        as HasqlPool
import qualified Hastile.Types.Logger              as Logger
import qualified Network.Wai                       as Wai
import qualified Network.Wai.Handler.Warp          as WaiWarp
import qualified Network.Wai.Middleware.Cors       as WaiCors
import qualified Network.Wai.Middleware.Prometheus as WaiPrometheus
import qualified Options.Generic                   as OptionsGeneric
import qualified Prometheus                        as Prometheus
import qualified Prometheus.Metric.GHC             as PrometheusGhc
import qualified STMContainers.Map                 as StmMap

import qualified Hastile.Config                    as Config
import qualified Hastile.Server                    as Server
import qualified Hastile.Types.App                 as App
import qualified Hastile.Types.Config              as Config
import qualified Hastile.Types.Layer               as Layer

main :: IO ()
main = OptionsGeneric.getRecord "hastile" >>= doIt

doIt :: Config.CmdLine -> IO ()
doIt cmdLine = do
  let cfgFile = Config.configFile cmdLine
  config <- Config.getConfig cfgFile
  doItWithConfig cfgFile config

doItWithConfig :: FilePath -> Config.Config -> IO ()
doItWithConfig cfgFile config@Config.Config{..} = do
  logEnv <- Logger.defaultLogEnv
  newTokenAuthorisationCache <- LRU.newLruHandle _configTokenCacheSize
  layers <- atomically StmMap.new :: IO (StmMap.Map OptionsGeneric.Text Layer.Layer)
  Foldable.forM_ (Map.toList _configLayers) $ \(k, v) -> atomically $ StmMap.insert (Layer.Layer k v) k layers
  ControlException.bracket
    (HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection))
    HasqlPool.release
    (\p -> getWarp _configPort (Server.runServer (App.ServerState p _configMapnikInputPlugins cfgFile config layers newTokenAuthorisationCache logEnv)))
  pure ()

getWarp :: WaiWarp.Port -> Wai.Application -> IO ()
getWarp port' app = do
  _ <- Prometheus.register PrometheusGhc.ghcMetrics
  let policy = WaiCors.simpleCorsResourcePolicy { WaiCors.corsRequestHeaders = ["Content-Type"] }
      application = WaiCors.cors (const $ Just policy) app
      promMiddleware = WaiPrometheus.prometheus $ WaiPrometheus.PrometheusSettings ["metrics"] True True
  WaiWarp.run port' $ promMiddleware application
