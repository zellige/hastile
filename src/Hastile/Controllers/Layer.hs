{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Controllers.Layer where

import           Control.Lens                  ((^.))
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader.Class    as RC
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encode.Pretty      as AE
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as LBS8
import qualified Data.Char                     as Char
import qualified Data.Geometry.GeoJsonToMvt    as GeoJsonToMvt
import qualified Data.Geometry.Types.Config    as TypesConfig
import qualified Data.Geometry.Types.Geography as TypesGeography
import qualified Data.Geospatial               as DG
import           Data.Map                      as M
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Read                as DTR
import           Data.Time
import           GHC.Conc
import           ListT
import           Network.HTTP.Types.Header     (hLastModified)
import           Numeric.Natural               (Natural)
import qualified Servant
import qualified STMContainers.Map             as STMMap

import qualified Hastile.DB.Layer              as DBLayer
import qualified Hastile.Lib.Layer             as LayerLib
import qualified Hastile.Routes                as Routes
import qualified Hastile.Types.App             as App
import qualified Hastile.Types.Config          as Config
import qualified Hastile.Types.Layer           as Layer
import qualified Hastile.Types.Layer.Security  as LayerSecurity

layerServer :: Servant.ServerT Routes.LayerApi App.ActionHandler
layerServer l = provisionLayer l Servant.:<|> serveLayer l

stmMapToList :: STMMap.Map k v -> STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STMMap.stream

createNewLayer :: Layer.LayerRequestList -> App.ActionHandler Servant.NoContent
createNewLayer (Layer.LayerRequestList layerRequests) =
  newLayer (\lastModifiedTime ls -> mapM_ (\lr -> STMMap.insert (Layer.requestToLayer (Layer._newLayerRequestName lr) (Layer._newLayerRequestSettings lr) lastModifiedTime) (Layer._newLayerRequestName lr) ls) layerRequests)

provisionLayer :: Text.Text -> Layer.LayerSettings -> App.ActionHandler Servant.NoContent
provisionLayer l settings =
  newLayer (\lastModifiedTime -> STMMap.insert (Layer.requestToLayer l settings lastModifiedTime) l)

newLayer :: (MonadIO m, RC.MonadReader App.ServerState m) => (UTCTime -> STMMap.Map Text.Text Layer.Layer -> STM a) -> m Servant.NoContent
newLayer b = do
  r <- RC.ask
  lastModifiedTime <- liftIO getCurrentTime
  let (ls, cfgFile, originalCfg) = (,,) <$> App._ssStateLayers <*> App._ssConfigFile <*> App._ssOriginalConfig $ r
  newLayers <- liftIO . atomically $ do
    _ <- b lastModifiedTime ls
    stmMapToList ls
  let newNewLayers = fmap (\(k, v) -> (k, Layer.layerToLayerDetails v)) newLayers
  liftIO $ LBS8.writeFile cfgFile (AE.encodePretty (originalCfg {Config._configLayers = fromList newNewLayers}))
  pure Servant.NoContent

serveLayer :: Text.Text -> Natural -> Natural -> Text.Text -> Maybe Text.Text -> Maybe Text.Text -> App.ActionHandler (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] BS.ByteString)
serveLayer l z x stringY maybeToken maybeIfModified = do
  layer <- getLayerOrThrow l
  pool <- RC.asks App._ssPool
  cache <- RC.asks App._ssTokenAuthorisationCache
  layerAuthorisation <- liftIO $ LayerLib.checkLayerAuthorisation pool cache layer maybeToken
  case layerAuthorisation of
    LayerSecurity.Authorised ->
      getContent z x stringY maybeIfModified layer
    LayerSecurity.Unauthorised ->
      throwError layerNotFoundError

getContent :: Natural -> Natural -> Text.Text -> Maybe Text.Text -> Layer.Layer -> App.ActionHandler (Servant.Headers '[Servant.Header "Last-Modified"  Text.Text] BS.ByteString)
getContent z x stringY maybeIfModified layer =
  if Layer.isModified layer maybeIfModified
      then getContent' layer z x stringY
      else throwError Servant.err304

getContent' :: Layer.Layer -> Natural -> Natural -> Text.Text -> App.ActionHandler (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] BS.ByteString)
getContent' l z x stringY
  | ".mvt" `Text.isSuffixOf` stringY = getAnything getTile l z x stringY
  | ".json" `Text.isSuffixOf` stringY = getAnything getJson l z x stringY
  | otherwise = throwError $ Servant.err400 { Servant.errBody = "Unknown request: " <> LBS8.fromStrict (TE.encodeUtf8 stringY) }

getAnything :: (t -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> App.ActionHandler a) -> t -> TypesGeography.ZoomLevel -> TypesGeography.Pixels -> Text.Text -> App.ActionHandler a
getAnything f layer z x stringY =
  case getY stringY of
    Left e       -> fail $ show e
    Right (y, _) -> f layer z (x, y)
  where
    getY s = DTR.decimal $ Text.takeWhile Char.isNumber s

getTile :: Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> App.ActionHandler (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] BS.ByteString)
getTile layer z xy = do
  buffer  <- RC.asks (^. App.ssBuffer)
  let simplificationAlgorithm = Layer.getAlgorithm z layer
      config = TypesConfig.mkConfig (Layer._layerName layer) z xy buffer Config.defaultTileSize (Layer.getLayerSetting layer Layer._layerQuantize) simplificationAlgorithm
  geoFeature <- getNewGeoFeature config layer z xy
  checkEmpty (GeoJsonToMvt.vtToBytes config geoFeature) layer

checkEmpty :: BS.ByteString -> Layer.Layer -> App.ActionHandler (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] BS.ByteString)
checkEmpty tile layer
  | BS.null tile = throwError $ App.err204 { Servant.errHeaders = [(hLastModified, TE.encodeUtf8 $ Layer.lastModified layer)] }
  | otherwise = pure $ Servant.addHeader (Layer.lastModified layer) tile

getJson :: Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> App.ActionHandler (Servant.Headers '[Servant.Header "Last-Modified" Text.Text] BS.ByteString)
getJson layer z xy = do
  buffer <- RC.asks (^. App.ssBuffer)
  let simplificationAlgorithm = Layer.getAlgorithm z layer
      config = TypesConfig.mkConfig (Layer._layerName layer) z xy buffer Config.defaultTileSize (Layer.getLayerSetting layer Layer._layerQuantize) simplificationAlgorithm
  Servant.addHeader (Layer.lastModified layer) . LBS8.toStrict . A.encode <$> getGeoFeature config layer z xy

getGeoFeature :: TypesConfig.Config -> Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> App.ActionHandler (DG.GeoFeatureCollection A.Value)
getGeoFeature config layer z xy = do
  errorOrTfs <- DBLayer.findFeatures config layer z xy
  case errorOrTfs of
    Left e    -> throwError $ Servant.err500 { Servant.errBody = LBS8.pack $ show e }
    Right tfs -> pure $ DG.GeoFeatureCollection Nothing tfs

getNewGeoFeature :: TypesConfig.Config -> Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> App.ActionHandler GeoJsonToMvt.StreamingLayer
getNewGeoFeature config layer z xy = do
  errorOrTfs <- DBLayer.newFindFeatures config layer z xy
  case errorOrTfs of
    Left e    -> throwError $ Servant.err500 { Servant.errBody = LBS8.pack $ show e }
    Right tfs -> pure tfs

getLayerOrThrow :: Text.Text -> App.ActionHandler Layer.Layer
getLayerOrThrow l = do
  errorOrLayer <- getLayer l
  case errorOrLayer of
    Left Layer.LayerNotFound -> throwError layerNotFoundError
    Right layer              -> pure layer

getLayer :: (MonadIO m, RC.MonadReader App.ServerState m) => Text.Text -> m (Either Layer.LayerError Layer.Layer)
getLayer l = do
  ls <- RC.asks App._ssStateLayers
  result <- liftIO . atomically $ STMMap.lookup l ls
  pure $ case result of
    Nothing    -> Left Layer.LayerNotFound
    Just layer -> Right layer

layerNotFoundError :: Servant.ServantErr
layerNotFoundError =
  Servant.err404 { Servant.errBody = "Layer not found :-(" }
