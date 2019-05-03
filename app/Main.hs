{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Control.Exception.Base            as ControlException
import qualified Control.Monad                     as Monad
import qualified Control.Monad.IO.Class            as MonadIO
import qualified Data.Foldable                     as Foldable
import qualified Data.LruCache.IO                  as LRU
import qualified Data.Map                          as Map
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as TextEncoding
import qualified Data.Time                         as Time
import           GHC.Conc
import qualified Hasql.Pool                        as HasqlPool
import qualified Katip
import qualified Network.Wai                       as Wai
import qualified Network.Wai.Handler.Warp          as WaiWarp
import qualified Network.Wai.Middleware.Cors       as WaiCors
import qualified Network.Wai.Middleware.Prometheus as WaiPrometheus
import qualified Options.Generic                   as OptionsGeneric
import qualified Prometheus
import qualified Prometheus.Metric.GHC             as PrometheusGhc
import qualified STMContainers.Map                 as StmMap

import qualified Hastile.Config                    as Config
import qualified Hastile.DB.Table                  as Table
import qualified Hastile.Server                    as Server
import qualified Hastile.Types.App                 as App
import qualified Hastile.Types.Config              as Config
import qualified Hastile.Types.Layer               as Layer
import qualified Hastile.Types.Logger              as Logger

main :: IO ()
main = OptionsGeneric.getRecord "hastile" >>= doIt

doIt :: Config.CmdLine -> IO ()
doIt cmdLine =
  case cmdLine of
    Config.Starter connection host port -> doItWithCommandLine connection host port
    Config.Server cfgFilePath -> do
      config <- Config.getConfig cfgFilePath
      doItWithConfig cfgFilePath config

doItWithConfig :: FilePath -> Config.Config -> IO ()
doItWithConfig cfgFile config@Config.Config{..} = do
  serverStartTime <- Time.getCurrentTime
  logEnv <- Logger.logHandler _configAppLog (Katip.Environment _configEnvironment)
  accessLogEnv <- Logger.logHandler _configAccessLog (Katip.Environment _configEnvironment)
  Table.checkConfig logEnv cfgFile config
  layerMetric <- registerLayerMetric
  newTokenAuthorisationCache <- LRU.newLruHandle _configTokenCacheSize
  layers <- initConfigLayers config
  let state p = App.StarterServerState p cfgFile config layers newTokenAuthorisationCache logEnv layerMetric serverStartTime
  ControlException.bracket
    (HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection))
    (cleanup [logEnv, accessLogEnv])
    (getWarp accessLogEnv _configPort . Server.runServer App.Authenticated . state)
  pure ()

doItWithCommandLine :: Text.Text -> Text.Text -> Int -> IO ()
doItWithCommandLine connection host port = do
  serverStartTime <- Time.getCurrentTime
  let inputConfig = Config.emptyInputConfig { Config._inputConfigPgConnection = connection, Config._inputConfigProtocolHost = Just host, Config._inputConfigPort = Just port }
      config@Config.Config{..} = Config.addDefaults inputConfig
      cfgFile = "config.json"
  getTables <- Table.getTables config
  logEnv <- Logger.logHandler _configAppLog (Katip.Environment _configEnvironment)
  accessLogEnv <- Logger.logHandler _configAccessLog (Katip.Environment _configEnvironment)
  layerMetric <- registerLayerMetric
  case getTables of
    Left _ -> undefined
    Right textLayers -> do
      let listLayers = fmap (\t -> (t, Layer.defaultLayerSettings)) textLayers
      layers <- initCmdLayers listLayers config
      Config.writeLayers listLayers config cfgFile
      let state p = App.ServerServerState p cfgFile config layers logEnv layerMetric serverStartTime
      ControlException.bracket
         (HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection))
         (cleanup [accessLogEnv])
         (getWarp accessLogEnv _configPort . Server.runServer App.Public . state)
      pure ()

initConfigLayers :: Config.Config -> IO (StmMap.Map Text.Text Layer.Layer)
initConfigLayers Config.Config{..} = do
  layers <- atomically StmMap.new :: IO (StmMap.Map OptionsGeneric.Text Layer.Layer)
  Foldable.forM_ (Map.toList _configLayers) $ \(k, v) -> atomically $ StmMap.insert (Layer.Layer k v) k layers
  pure layers

initCmdLayers :: [(Text.Text, Layer.LayerSettings)] -> Config.Config -> IO (StmMap.Map Text.Text Layer.Layer)
initCmdLayers newLayers Config.Config{..} = do
  layers <- atomically StmMap.new :: IO (StmMap.Map OptionsGeneric.Text Layer.Layer)
  Foldable.forM_ newLayers $ \(k, v) -> atomically $ StmMap.insert (Layer.Layer k v) k layers
  pure layers

cleanup :: [Katip.LogEnv] -> HasqlPool.Pool -> IO ()
cleanup logEnvs pool = do
  _ <- HasqlPool.release pool
  _ <- Monad.mapM_ Katip.closeScribes logEnvs
  pure ()

getWarp :: Katip.LogEnv -> WaiWarp.Port -> Wai.Application -> IO ()
getWarp logEnv port' app = do
  _ <- Prometheus.register PrometheusGhc.ghcMetrics
  let policy = WaiCors.simpleCorsResourcePolicy { WaiCors.corsRequestHeaders = ["Content-Type"] }
      application = WaiCors.cors (const $ Just policy) app
      logging = waiRequestLogger logEnv
      promMiddleware = WaiPrometheus.prometheus $ WaiPrometheus.PrometheusSettings ["metrics"] True True
  WaiWarp.run port' . promMiddleware $ logging application

waiRequestLogger :: Katip.LogEnv -> Wai.Middleware
waiRequestLogger env app req respond =
  app req $ \res -> do
    currentTime <- MonadIO.liftIO Time.getCurrentTime
    Logger.apacheLog env currentTime req res
    respond res

{-# NOINLINE registerLayerMetric #-}
registerLayerMetric :: (MonadIO.MonadIO m) => m (Prometheus.Vector (Text.Text, Text.Text) Prometheus.Counter)
registerLayerMetric = Prometheus.register
            $ Prometheus.vector ("token", "layer")
            $ Prometheus.counter
            $ Prometheus.Info "layers_by_token" "Count of layer views by token."


