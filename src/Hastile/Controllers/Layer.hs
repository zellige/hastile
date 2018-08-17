{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Controllers.Layer where

import           Control.Lens               ((^.))
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader.Class as RC
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encode.Pretty   as AE
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Char                  as C
import qualified Data.Geometry.Types.Types  as DGTT
import qualified Data.Geospatial            as DG
import           Data.Map                   as M
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Read             as DTR
import           Data.Time
import           GHC.Conc
import           ListT
import           Network.HTTP.Types.Header  (hLastModified)
import           Numeric.Natural            (Natural)
import qualified Servant                    as S
import qualified STMContainers.Map          as STM

import qualified Hastile.DB.Layer           as DB
import qualified Hastile.Routes             as Routes
import qualified Hastile.Tile               as Tile
import qualified Hastile.Types.App          as App
import qualified Hastile.Types.Config       as Config
import qualified Hastile.Types.Layer        as Layer


layerServer :: S.ServerT Routes.LayerApi App.ActionHandler
layerServer l = provisionLayer l S.:<|> coordsServer l

coordsServer :: T.Text -> Natural -> Natural -> S.ServerT Routes.HastileContentApi App.ActionHandler
coordsServer l z x = getQuery l z x S.:<|> getContent l z x

stmMapToList :: STM.Map k v -> STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STM.stream

createNewLayer :: Layer.LayerRequestList -> App.ActionHandler S.NoContent
createNewLayer (Layer.LayerRequestList layerRequests) =
  newLayer (\lastModifiedTime ls -> mapM_ (\lr -> STM.insert (Layer.requestToLayer (Layer._newLayerRequestName lr) (Layer._newLayerRequestSettings lr) lastModifiedTime) (Layer._newLayerRequestName lr) ls) layerRequests)

provisionLayer :: T.Text -> Layer.LayerSettings -> App.ActionHandler S.NoContent
provisionLayer l settings =
  newLayer (\lastModifiedTime -> STM.insert (Layer.requestToLayer l settings lastModifiedTime) l)

newLayer :: (MonadIO m, RC.MonadReader App.ServerState m) => (UTCTime -> STM.Map T.Text Layer.Layer -> STM a) -> m S.NoContent
newLayer b = do
  r <- RC.ask
  lastModifiedTime <- liftIO getCurrentTime
  let (ls, cfgFile, originalCfg) = (,,) <$> App._ssStateLayers <*> App._ssConfigFile <*> App._ssOriginalConfig $ r
  newLayers <- liftIO . atomically $ do
    _ <- b lastModifiedTime ls
    stmMapToList ls
  let newNewLayers = fmap (\(k, v) -> (k, Layer.layerToLayerDetails v)) newLayers
  liftIO $ LBS8.writeFile cfgFile (AE.encodePretty (originalCfg {Config._configLayers = fromList newNewLayers}))
  pure S.NoContent

getQuery :: T.Text -> Natural -> Natural -> Natural -> App.ActionHandler T.Text
getQuery l z x y = do
  layer <- getLayerOrThrow l
  DB.mkQuery layer z (x, y)

getContent :: T.Text -> Natural -> Natural -> T.Text -> Maybe T.Text -> App.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
getContent l z x stringY maybeIfModified =
  do
    layer <- getLayerOrThrow l
    if DB.isModified layer maybeIfModified
      then getContent' layer z x stringY
      else throwError S.err304

getContent' :: Layer.Layer -> Natural -> Natural -> T.Text -> App.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
getContent' l z x stringY
  | ".mvt" `T.isSuffixOf` stringY = getAnything getTile l z x stringY
  | ".json" `T.isSuffixOf` stringY = getAnything getJson l z x stringY
  | otherwise = throwError $ S.err400 { S.errBody = "Unknown request: " <> LBS8.fromStrict (TE.encodeUtf8 stringY) }

getAnything :: (t -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> App.ActionHandler a) -> t -> DGTT.ZoomLevel -> DGTT.Pixels -> T.Text -> App.ActionHandler a
getAnything f layer z x stringY =
  case getY stringY of
    Left e       -> fail $ show e
    Right (y, _) -> f layer z (x, y)
  where
    getY s = DTR.decimal $ T.takeWhile C.isNumber s

getTile :: Layer.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> App.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
getTile layer z xy = do
  geoJson <- getJson' layer z xy
  buffer  <- RC.asks (^. App.ssBuffer)
  let simplificationAlgorithm = Layer.getAlgorithm z layer
  tile    <- liftIO $ Tile.mkTile (Layer._layerName layer) z xy buffer (Layer._layerQuantize layer) simplificationAlgorithm geoJson
  checkEmpty tile layer

checkEmpty :: BS.ByteString -> Layer.Layer -> App.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
checkEmpty tile layer
  | BS.null tile = throwError $ App.err204 { S.errHeaders = [(hLastModified, TE.encodeUtf8 $ DB.lastModified layer)] }
  | otherwise = pure $ S.addHeader (DB.lastModified layer) tile

getJson :: Layer.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> App.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
getJson layer z xy = S.addHeader (DB.lastModified layer) . LBS8.toStrict . A.encode <$> getJson' layer z xy

getJson' :: Layer.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> App.ActionHandler (DG.GeoFeatureCollection A.Value)
getJson' layer z xy = do
  errorOrTfs <- DB.findFeatures layer z xy
  case errorOrTfs of
    Left e    -> throwError $ S.err500 { S.errBody = LBS8.pack $ show e }
    Right tfs -> pure $ DG.GeoFeatureCollection Nothing (Layer.mkGeoJSON tfs)

getLayerOrThrow :: T.Text -> App.ActionHandler Layer.Layer
getLayerOrThrow l = do
  errorOrLayer <- DB.getLayer l
  case errorOrLayer of
    Left DB.LayerNotFound -> throwError $ S.err404 { S.errBody = "Layer not found :-(" }
    Right layer -> pure layer
