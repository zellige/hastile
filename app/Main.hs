{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Control.Exception.Base            as ControlException
import qualified Data.ByteString                   as ByteString
import qualified Data.Foldable                     as Foldable
import qualified Data.LruCache.IO                  as LRU
import qualified Data.Map                          as Map
import qualified Data.Maybe                        as Maybe
import           Data.Monoid                       ((<>))
import qualified Data.Text.Encoding                as TextEncoding
import           GHC.Conc
import qualified Hasql.Pool                        as HasqlPool
import qualified Hastile.Types.Logger              as Logger
import qualified Katip
import qualified Network.HTTP.Types                as HttpTypes
import qualified Network.HTTP.Types.Header         as HttpTypesHeaders
import qualified Network.Wai                       as Wai
import qualified Network.Wai.Handler.Warp          as WaiWarp
import qualified Network.Wai.Header                as WaiHeader
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
  logEnv <- Logger.defaultLogEnv (Katip.Environment _configEnvironment)
  newTokenAuthorisationCache <- LRU.newLruHandle _configTokenCacheSize
  layers <- atomically StmMap.new :: IO (StmMap.Map OptionsGeneric.Text Layer.Layer)
  Foldable.forM_ (Map.toList _configLayers) $ \(k, v) -> atomically $ StmMap.insert (Layer.Layer k v) k layers
  let state p = App.ServerState p cfgFile config layers newTokenAuthorisationCache logEnv
  ControlException.bracket
    (HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection))
    (cleanup logEnv)
    (getWarp logEnv _configPort . Server.runServer . state)
  pure ()

cleanup :: Katip.LogEnv -> HasqlPool.Pool -> IO ()
cleanup logEnv pool = do
  _ <- HasqlPool.release pool
  _ <- Katip.closeScribes logEnv
  pure ()

getWarp :: Katip.LogEnv -> WaiWarp.Port -> Wai.Application -> IO ()
getWarp logEnv port' app = do
  _ <- Prometheus.register PrometheusGhc.ghcMetrics
  let policy = WaiCors.simpleCorsResourcePolicy { WaiCors.corsRequestHeaders = ["Content-Type"] }
      application = WaiCors.cors (const $ Just policy) app
      logging = katipLogger logEnv
      promMiddleware = WaiPrometheus.prometheus $ WaiPrometheus.PrometheusSettings ["metrics"] True True
  WaiWarp.run port' . promMiddleware $ logging application

katipLogger :: Katip.LogEnv -> Wai.Middleware
katipLogger env app req respond =
  app req $ \res -> do
    let responseHeaders = Wai.responseHeaders res
        requestHeaders = Wai.requestHeaders req
        sourceIp = show $ TextEncoding.decodeUtf8 $ Maybe.fromMaybe "" (getSource requestHeaders)
        formattedTime = ""
        requestMethod = show $ Wai.requestMethod req
        path = show $ Wai.rawPathInfo req <> Wai.rawQueryString req
        httpVersion = show (Wai.httpVersion req)
        status = show . HttpTypes.statusCode $ Wai.responseStatus res
        contentLength = Maybe.maybe "-" show (WaiHeader.contentLength responseHeaders)
        mr = Maybe.maybe "-" show (Wai.requestHeaderReferer req)
        mua = Maybe.maybe "-" show (Wai.requestHeaderUserAgent req)
        finalStr =
          sourceIp    <> " - - ["    <> formattedTime <> "] " <> requestMethod <> " " <> path <> " " <>
          httpVersion <> " "         <> status        <> " "  <> contentLength <> " " <> mr   <> " " <>
          mua
    s <- respond res
    Katip.runKatipT env $
      Katip.logMsg "web" Katip.InfoS (Katip.logStr finalStr)
    pure s

getSource :: HttpTypesHeaders.RequestHeaders -> Maybe ByteString.ByteString
getSource hdrs = fmap snd maddr
  where
    maddr = Foldable.find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
