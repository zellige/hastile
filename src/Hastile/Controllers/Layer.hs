{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Controllers.Layer
  ( createNewLayer
  , layerServer) where

import           Control.Lens                        ((^.))
import           Control.Monad.Error.Class
import qualified Control.Monad.IO.Class              as MonadIO
import qualified Control.Monad.Logger                as MonadLogger
import qualified Control.Monad.Reader.Class          as ReaderClass
import qualified Data.Aeson                          as Aeson
import qualified Data.ByteString                     as ByteString
import qualified Data.ByteString.Lazy.Char8          as ByteStringLazyChar8
import qualified Data.Char                           as Char
import qualified Data.Geometry.GeoJsonStreamingToMvt as GeoJsonStreamingToMvt
import qualified Data.Geometry.Types.Config          as TypesConfig
import qualified Data.Geometry.Types.Geography       as TypesGeography
import qualified Data.Geometry.Types.MvtFeatures     as TypesMvtFeatures
import qualified Data.Geospatial                     as Geospatial
import qualified Data.Maybe                          as Maybe
import           Data.Monoid                         ((<>))
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as TextEncoding
import qualified Data.Text.Read                      as TextRead
import qualified Data.Time                           as Time
import           GHC.Conc
import           Network.HTTP.Types.Header           (hLastModified)
import           Numeric.Natural                     (Natural)
import qualified Prometheus
import qualified Servant
import qualified STMContainers.Map                   as STMMap

import qualified Hastile.DB.Layer                    as DBLayer
import qualified Hastile.Lib.Layer                   as LayerLib
import qualified Hastile.Lib.Tile                    as TileLib
import qualified Hastile.Routes                      as Routes
import qualified Hastile.Types.App                   as App
import qualified Hastile.Types.Config                as Config
import qualified Hastile.Types.Layer                 as Layer
import qualified Hastile.Types.Layer.Format          as LayerFormat
import qualified Hastile.Types.Layer.Security        as LayerSecurity


layerServer :: (MonadIO.MonadIO m) => Servant.ServerT Routes.LayerApi (App.ActionHandler m)
layerServer = createNewLayer Servant.:<|>
  (\l -> provisionLayer l Servant.:<|> serveLayer l Servant.:<|> serveTileJson l)

createNewLayer :: (MonadIO.MonadIO m) => Layer.LayerRequestList -> App.ActionHandler m Servant.NoContent
createNewLayer (Layer.LayerRequestList layerRequests) = do
  lastModifiedTime <- MonadIO.liftIO Time.getCurrentTime
  let layersToAdd = fmap (\l -> Layer.requestToLayer (Layer._newLayerRequestName l) (Layer._newLayerRequestSettings l) lastModifiedTime) layerRequests
  mapM_ (\l -> MonadLogger.logInfoNS "web" ("Adding layer " <> Layer._layerName l)) layersToAdd
  newLayer layersToAdd
  pure Servant.NoContent

provisionLayer :: (MonadIO.MonadIO m) => Text.Text -> Layer.LayerSettings -> App.ActionHandler m Servant.NoContent
provisionLayer l settings = do
  lastModifiedTime <- MonadIO.liftIO Time.getCurrentTime
  let layerToModify = Layer.requestToLayer l settings lastModifiedTime
  MonadLogger.logInfoNS "web" ("Modify layer " <> Layer._layerName layerToModify)
  newLayer [layerToModify]
  pure Servant.NoContent

serveLayer :: (MonadIO.MonadIO m) => Text.Text -> Natural -> Natural -> Text.Text -> Maybe Text.Text -> Maybe Text.Text -> App.ActionHandler m (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] ByteString.ByteString)
serveLayer l z x stringY maybeToken maybeIfModified = do
  layer <- getLayerOrThrow l
  layerCount <- ReaderClass.asks App._ssLayerMetric
  pool <- ReaderClass.asks App._ssPool
  cache <- ReaderClass.asks App._ssTokenAuthorisationCache
  layerAuthorisation <- MonadIO.liftIO $ LayerLib.checkLayerAuthorisation pool cache layer maybeToken
  case layerAuthorisation of
    LayerSecurity.Authorised -> do
      let token = Maybe.fromMaybe "" maybeToken
      _ <- MonadIO.liftIO $ Prometheus.withLabel layerCount (token, Layer._layerName layer) Prometheus.incCounter
      getContent z x stringY maybeIfModified layer
    LayerSecurity.Unauthorised ->
      throwError layerNotFoundError

newLayer :: (MonadIO.MonadIO m) => [Layer.Layer] -> App.ActionHandler m ()
newLayer layers = do
  r <- ReaderClass.ask
  let (ls, cfgFile, originalCfg) = (,,) <$> App._ssStateLayers <*> App._ssConfigFile <*> App._ssOriginalConfig $ r
  newLayers <- Config.addLayers layers ls
  Config.writeLayers newLayers originalCfg cfgFile
  pure ()

serveTileJson :: (MonadIO.MonadIO m) => Text.Text -> App.ActionHandler m Layer.LayerDetails
serveTileJson layerName
  | ".json" `Text.isSuffixOf` layerName = do
                    layer <- getLayerOrThrow layerName
                    pure $ Layer.layerToLayerDetails layer
  | otherwise = throwError $ Servant.err400 { Servant.errBody = "Unknown request: " <> ByteStringLazyChar8.fromStrict (TextEncoding.encodeUtf8 layerName) }


getContent :: (MonadIO.MonadIO m) => Natural -> Natural -> Text.Text -> Maybe Text.Text -> Layer.Layer -> App.ActionHandler m (Servant.Headers '[Servant.Header "Last-Modified"  Text.Text] ByteString.ByteString)
getContent z x stringY maybeIfModified layer =
  if Layer.isModified layer maybeIfModified
      then getContent' layer z x stringY
      else throwError Servant.err304

getContent' :: (MonadIO.MonadIO m) => Layer.Layer -> Natural -> Natural -> Text.Text -> App.ActionHandler m (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] ByteString.ByteString)
getContent' l z x stringY
  | (".mvt" `Text.isSuffixOf` stringY) || (".pbf" `Text.isSuffixOf` stringY) || (".vector.pbf" `Text.isSuffixOf` stringY) = getAnything getTile l z x stringY
  | ".json" `Text.isSuffixOf` stringY = getAnything getJson l z x stringY
  | otherwise = throwError $ Servant.err400 { Servant.errBody = "Unknown request: " <> ByteStringLazyChar8.fromStrict (TextEncoding.encodeUtf8 stringY) }

getAnything :: (MonadIO.MonadIO m) => (t -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> App.ActionHandler m a) -> t -> TypesGeography.ZoomLevel -> TypesGeography.Pixels -> Text.Text -> App.ActionHandler m a
getAnything f layer z x stringY =
  case getY stringY of
    Left e       -> fail $ show e
    Right (y, _) -> f layer z (x, y)
  where
    getY s = TextRead.decimal $ Text.takeWhile Char.isNumber s

getTile :: (MonadIO.MonadIO m) => Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> App.ActionHandler m (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] ByteString.ByteString)
getTile layer z xy = do
  buffer  <- ReaderClass.asks (^. App.ssBuffer)
  let simplificationAlgorithm = Layer.getAlgorithm z layer
      config = TypesConfig.mkConfig (Layer._layerName layer) z xy buffer Config.defaultTileSize (Layer.getLayerSetting layer Layer._layerQuantize) simplificationAlgorithm
      layerFormat = Layer.getLayerSetting layer Layer._layerFormat
  case layerFormat of
    LayerFormat.GeoJSON -> do
      geoFeature <- getGeoFeature layer z xy
      tile <- MonadIO.liftIO $ TileLib.mkTile (Layer._layerName layer) z xy buffer (Layer.getLayerSetting layer Layer._layerQuantize) simplificationAlgorithm geoFeature
      checkEmpty tile layer
    LayerFormat.WkbProperties -> do
      geoFeature <- getStreamingLayer config layer z xy
      checkEmpty (GeoJsonStreamingToMvt.vtToBytes config geoFeature) layer

checkEmpty :: (MonadIO.MonadIO m) => ByteString.ByteString -> Layer.Layer -> App.ActionHandler m (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] ByteString.ByteString)
checkEmpty tile layer
  | ByteString.null tile = throwError $ App.err204 { Servant.errHeaders = [(hLastModified, TextEncoding.encodeUtf8 $ Layer.lastModifiedFromLayer layer)] }
  | otherwise = pure $ Servant.addHeader (Layer.lastModifiedFromLayer layer) tile

getJson :: (MonadIO.MonadIO m) => Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) ->  App.ActionHandler m (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] ByteString.ByteString)
getJson layer z xy = Servant.addHeader (Layer.lastModifiedFromLayer layer) . ByteStringLazyChar8.toStrict . Aeson.encode <$> getGeoFeature layer z xy

getGeoFeature :: (MonadIO.MonadIO m) => Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> App.ActionHandler m (Geospatial.GeoFeatureCollection Aeson.Value)
getGeoFeature layer z xy = do
  errorOrTfs <- DBLayer.findFeatures layer z xy
  case errorOrTfs of
    Left e    -> throwError $ Servant.err500 { Servant.errBody = ByteStringLazyChar8.pack $ show e }
    Right tfs -> pure $ Geospatial.GeoFeatureCollection Nothing tfs

getStreamingLayer :: (MonadIO.MonadIO m) => TypesConfig.Config -> Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> App.ActionHandler m TypesMvtFeatures.StreamingLayer
getStreamingLayer config layer z xy = do
  errorOrTfs <- DBLayer.findFeaturesStreaming config layer z xy
  case errorOrTfs of
    Left e    -> throwError $ Servant.err500 { Servant.errBody = ByteStringLazyChar8.pack $ show e }
    Right tfs -> pure tfs

getLayerOrThrow :: (MonadIO.MonadIO m) => Text.Text -> App.ActionHandler m Layer.Layer
getLayerOrThrow l = do
  errorOrLayer <- getLayer l
  case errorOrLayer of
    Left Layer.LayerNotFound -> throwError layerNotFoundError
    Right layer              -> pure layer

getLayer :: (MonadIO.MonadIO m) => Text.Text -> App.ActionHandler m (Either Layer.LayerError Layer.Layer)
getLayer l = do
  ls <- ReaderClass.asks App._ssStateLayers
  result <- MonadIO.liftIO . atomically $ STMMap.lookup l ls
  pure $ case result of
    Nothing    -> Left Layer.LayerNotFound
    Just layer -> Right layer

layerNotFoundError :: Servant.ServantErr
layerNotFoundError =
  Servant.err404 { Servant.errBody = "Layer not found :-(" }
