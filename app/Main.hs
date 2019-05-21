{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Control.Exception.Base            as ControlException
import qualified Control.Monad                     as Monad
import qualified Control.Monad.Except              as Except
import qualified Control.Monad.IO.Class            as MonadIO
import qualified Data.Foldable                     as Foldable
import qualified Data.LruCache.IO                  as LRU
import qualified Data.Map                          as Map
import qualified Data.Maybe                        as Maybe
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
import qualified System.Directory                  as SystemDirectory

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
    Config.Starter cfgFilePath dbConnection host port -> do
      (newCfgFilePath, newConfig) <- setupLayersConfiguration cfgFilePath dbConnection host port
      doItWithConfig newCfgFilePath newConfig (App.StarterServerState newCfgFilePath newConfig) Table.nullCheckConfig
    Config.Server cfgFilePath -> do
      config <- Config.getConfig cfgFilePath
      cache <- LRU.newLruHandle (Config._configTokenCacheSize config)
      layerMetric <- registerLayerMetric
      doItWithConfig cfgFilePath config (App.ServerServerState cfgFilePath config cache layerMetric) Table.checkConfig

doItWithConfig :: FilePath -> Config.Config -> App.SetupAppState -> Table.RunCheckConfig -> IO ()
doItWithConfig cfgFile config@Config.Config{..} appState checkConfig = do
  serverStartTime <- Time.getCurrentTime
  logEnv <- Logger.logHandler _configAppLog (Katip.Environment _configEnvironment)
  accessLogEnv <- Logger.logHandler _configAccessLog (Katip.Environment _configEnvironment)
  checkConfig logEnv cfgFile config
  layers <- initConfigLayers config
  let state = appState serverStartTime logEnv layers
  ControlException.bracket
      (HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection))
      (cleanup [accessLogEnv])
      (getWarp accessLogEnv _configPort . Server.runServer . state)
  pure ()

setupLayersConfiguration :: Maybe FilePath -> Text.Text -> Text.Text -> Int -> IO (FilePath, Config.Config)
setupLayersConfiguration maybeCfgFile dbConnection host port =
  let
    config = createConfig dbConnection host port
    configFileName = Maybe.fromMaybe "hastile-config.json" maybeCfgFile
  in do
    fileExists <- SystemDirectory.doesFileExist configFileName
    if fileExists then do
      readConfig <- Config.getConfig configFileName
      pure (configFileName, readConfig)
    else do
      maybeNewConfig <- MonadIO.liftIO . Except.runExceptT $ Table.writeConfigFromDatabaseTables configFileName config
      case maybeNewConfig of
        Left err        -> do
          MonadIO.liftIO $ print err
          pure (configFileName, config)
        Right newConfig -> pure (configFileName, newConfig)

writeConfigFromDatabaseTables :: FilePath -> Text.Text -> Text.Text -> Int -> Except.ExceptT Text.Text IO Config.Config
writeConfigFromDatabaseTables cfgFile dbConnection host port = do
  let config = createConfig dbConnection host port
  textLayers <- MonadIO.liftIO (Table.getTables config) >>= Except.liftEither
  boxes <- MonadIO.liftIO (Table.getBboxes config textLayers) >>= Except.liftEither
  let listLayers = zipWith (\b t -> (t, Layer.defaultLayerSettings { Layer._layerBounds = b } )) boxes textLayers
  Config.writeLayers listLayers config cfgFile
  MonadIO.liftIO $ Config.getConfig cfgFile

createConfig :: Text.Text -> Text.Text -> Int -> Config.Config
createConfig dbConnection host port = Config.addDefaults inputConfig
  where
    inputConfig = Config.emptyInputConfig { Config._inputConfigPgConnection = dbConnection, Config._inputConfigProtocolHost = Just host, Config._inputConfigPort = Just port }

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


