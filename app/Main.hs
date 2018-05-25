{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Control.Exception.Base            as ControlException
import qualified Data.Foldable                     as Foldable
import qualified Data.Map                          as Map
import qualified Data.Text.Encoding                as TextEncoding
import           GHC.Conc
import qualified Hasql.Pool                        as HasqlPool
import qualified Network.Wai                       as Wai
import qualified Network.Wai.Handler.Warp          as WaiWarp
import qualified Network.Wai.Middleware.Cors       as WaiCors
import qualified Network.Wai.Middleware.Prometheus as WaiPrometheus
import qualified Options.Generic                   as OptionsGeneric
import qualified Prometheus                        as Prometheus
import qualified Prometheus.Metric.GHC             as PrometheusGhc
import qualified STMContainers.Map                 as StmMap

import qualified Config                            as Config
import qualified Server                            as Server
import qualified Types                             as Types

main :: IO ()
main = OptionsGeneric.getRecord "hastile" >>= doIt

doIt :: Types.CmdLine -> IO ()
doIt cmdLine = do
  let cfgFile = Types.configFile cmdLine
  config <- Config.getConfig cfgFile
  doItWithConfig cfgFile config

doItWithConfig :: FilePath -> Types.Config -> IO ()
doItWithConfig cfgFile config@Types.Config{..} = do
  layers <- atomically StmMap.new :: IO (StmMap.Map OptionsGeneric.Text Types.Layer)
  Foldable.forM_ (Map.toList _configLayers) $ \(k, v) -> atomically $ StmMap.insert (Types.layerDetailsToLayer k v) k layers
  ControlException.bracket
    (HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection))
    HasqlPool.release
    (\p -> getWarp _configPort (Server.runServer (Types.ServerState p _configMapnikInputPlugins cfgFile config layers)))
  pure ()

getWarp :: WaiWarp.Port -> Wai.Application -> IO ()
getWarp port' app = do
  _ <- Prometheus.register PrometheusGhc.ghcMetrics
  let application = WaiCors.cors (const $ Just policy) app
      promMiddleware = WaiPrometheus.prometheus $ WaiPrometheus.PrometheusSettings ["metrics"] True True
  WaiWarp.run port' $ promMiddleware $ application
  where
    policy = WaiCors.simpleCorsResourcePolicy { WaiCors.corsRequestHeaders = ["Content-Type"] }
