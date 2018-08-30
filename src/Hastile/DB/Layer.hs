{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Layer where

import           Control.Lens                  ((^.))
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as AesonTypes
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LazyByteString
import qualified Data.Functor.Contravariant    as Contravariant
import qualified Data.Geometry.Types.Geography as DGTT
import qualified Data.Geospatial               as Geospatial
import           Data.Monoid
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TE
import qualified Data.Time                     as DT
import qualified Data.Wkb                      as Wkb
import           GHC.Conc
import qualified Hasql.Decoders                as HD
import qualified Hasql.Encoders                as HE
import qualified Hasql.Pool                    as P
import qualified Hasql.Query                   as HQ
import qualified Hasql.Transaction             as Transaction
import qualified Hasql.Transaction.Sessions    as Transaction
import           STMContainers.Map             as STM

import           Hastile.Tile
import qualified Hastile.Tile                  as Tile
import qualified Hastile.Types.App             as App
import qualified Hastile.Types.Config          as Config
import qualified Hastile.Types.Layer           as Layer
-- import qualified Hastile.Types.Layer.Format    as LayerFormat

data LayerError = LayerNotFound

findFeatures :: (MonadIO m, MonadReader App.ServerState m)
             => Layer.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> m (Either P.UsageError [Geospatial.GeoFeature AesonTypes.Value])
findFeatures layer z xy = do
  buffer <- asks (^. App.ssBuffer)
  let bboxM = googleToBBoxM Config.defaultTileSize z xy
      bbox = addBufferToBBox Config.defaultTileSize buffer z bboxM
      tableName = Layer.getLayerSetting layer Layer._layerTableName
      query = case Layer.getLayerSetting layer Layer._layerFormat of
                -- LayerFormat.GeoJSON ->
                --   layerQueryJSON tableName
                -- LayerFormat.WkbProperteis ->
                _ -> layerQueryWkbProperties tableName
      action = Transaction.query bbox query
      session = Transaction.transaction
              Transaction.ReadCommitted Transaction.Read action
  hpool <- asks App._ssPool
  liftIO $ P.use hpool session

layerQueryEncoder :: HE.Params (BBox Metres)
layerQueryEncoder =
  Contravariant.contramap Tile._bboxLlx (HE.value metreValue)
  <> Contravariant.contramap Tile._bboxLly (HE.value metreValue)
  <> Contravariant.contramap Tile._bboxUrx (HE.value metreValue)
  <> Contravariant.contramap Tile._bboxUry (HE.value metreValue)

metreValue :: HE.Value Tile.Metres
metreValue =
  Contravariant.contramap metreTodouble HE.float8
  where metreTodouble (Tile.Metres double) = double

layerQueryJSON :: Text.Text -> HQ.Query (BBox Metres) [Aeson.Value]
layerQueryJSON tableName =
    HQ.statement sql layerQueryEncoder (HD.rowsList (HD.value HD.json)) False
  where
    sql = TE.encodeUtf8 $ Text.pack $ "SELECT geojson FROM " ++ Text.unpack  tableName ++ layerQueryWhereClause

layerQueryWkbProperties :: Text.Text -> HQ.Query (BBox Metres) [Geospatial.GeoFeature AesonTypes.Value]
layerQueryWkbProperties tableName =
    HQ.statement sql layerQueryEncoder (HD.rowsList wkbPropertiesDecoder) False
  where
    sql = TE.encodeUtf8 $ Text.pack $ "SELECT ST_AsBinary(wkb_geometry) FROM " ++ Text.unpack tableName ++ layerQueryWhereClause

wkbPropertiesDecoder :: HD.Row (Geospatial.GeoFeature AesonTypes.Value)
wkbPropertiesDecoder =
  HD.value $ HD.custom (\_ -> either (Left . Text.pack) (\g -> Right $ Geospatial.GeoFeature Nothing g AesonTypes.Null Nothing) . Wkb.parseByteString . LazyByteString.fromStrict)

layerQueryWhereClause :: String
layerQueryWhereClause =
  " WHERE ST_Intersects(wkb_geometry, ST_Transform(ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1, $2), ST_MakePoint($3, $4)), 3857), 4326));"

getLayer :: (MonadIO m, MonadReader App.ServerState m) => Text.Text -> m (Either LayerError Layer.Layer)
getLayer l = do
  ls <- asks App._ssStateLayers
  result <- liftIO . atomically $ STM.lookup l ls
  pure $ case result of
    Nothing    -> Left LayerNotFound
    Just layer -> Right layer

mkStatement :: BS.ByteString -> HQ.Query () [Aeson.Value]
mkStatement sql = HQ.statement sql
    HE.unit (HD.rowsList (HD.value HD.json)) False

lastModified :: Layer.Layer -> Text.Text
lastModified layer = Text.dropEnd 3 (Text.pack rfc822Str) <> "GMT"
       where rfc822Str = DT.formatTime DT.defaultTimeLocale DT.rfc822DateFormat $ Layer.getLayerDetail layer Layer._layerLastModified

parseIfModifiedSince :: Text.Text -> Maybe DT.UTCTime
parseIfModifiedSince t = DT.parseTimeM True DT.defaultTimeLocale "%a, %e %b %Y %T GMT" $ Text.unpack t

isModifiedTime :: Layer.Layer -> Maybe DT.UTCTime -> Bool
isModifiedTime layer mTime =
  case mTime of
    Nothing   -> True
    Just time -> Layer.getLayerDetail layer Layer._layerLastModified > time

isModified :: Layer.Layer -> Maybe Text.Text -> Bool
isModified layer mText =
  case mText of
    Nothing   -> True
    Just text -> isModifiedTime layer $ parseIfModifiedSince text
