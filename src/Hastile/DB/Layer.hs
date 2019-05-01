{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Layer where

import           Control.Lens                        ((^.))
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import qualified Data.Aeson                          as Aeson
import qualified Data.Aeson.Types                    as AesonTypes
import qualified Data.ByteString                     as ByteString
import qualified Data.ByteString.Lazy                as LazyByteString
import qualified Data.Ewkb                           as Ewkb
import qualified Data.Geometry.GeoJsonStreamingToMvt as GeoJsonStreamingToMvt
import qualified Data.Geometry.MapnikVectorTile      as MapnikVectorTile
import qualified Data.Geometry.Types.Config          as TypesConfig
import qualified Data.Geometry.Types.Geography       as TypesGeography
import qualified Data.Geometry.Types.MvtFeatures     as TypesMvtFeatures
import qualified Data.Geospatial                     as Geospatial
import           Data.Monoid                         ((<>))
import qualified Data.Sequence                       as Sequence
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as TextEncoding
import qualified Hasql.CursorQuery                   as HasqlCursorQuery
import qualified Hasql.CursorQuery.Transactions      as HasqlCursorQueryTransactions
import qualified Hasql.Decoders                      as HasqlDecoders
import qualified Hasql.Encoders                      as HasqlEncoders
import qualified Hasql.Pool                          as HasqlPool
import qualified Hasql.Statement                     as HasqlStatement
import qualified Hasql.Transaction                   as HasqlTransaction
import qualified Hasql.Transaction.Sessions          as HasqlTransactionSession

import qualified Hastile.DB                          as DB
import qualified Hastile.Lib.Tile                    as TileLib
import qualified Hastile.Types.App                   as App
import qualified Hastile.Types.Layer                 as Layer
import qualified Hastile.Types.Tile                  as Tile

-- API
findFeatures :: (MonadIO m, MonadReader App.ServerState m) => Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> m (Either HasqlPool.UsageError (Sequence.Seq (Geospatial.GeoFeature AesonTypes.Value)))
findFeatures layer z xy = do
  buffer <- asks (^. App.ssBuffer)
  hpool <- asks App._ssPool
  let bbox = TileLib.getBbox buffer z xy
      tableName = Layer.getLayerSetting layer Layer._layerTableName
      query = layerQueryGeoJSON tableName
      action = HasqlCursorQueryTransactions.cursorQuery bbox query
      session = HasqlTransactionSession.transaction HasqlTransactionSession.ReadCommitted HasqlTransactionSession.Read action
  liftIO $ HasqlPool.use hpool session

findFeaturesStreaming :: (MonadIO m, MonadReader App.ServerState m) => TypesConfig.Config -> Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> m (Either HasqlPool.UsageError TypesMvtFeatures.StreamingLayer)
findFeaturesStreaming config layer z xy = do
  buffer <- asks (^. App.ssBuffer)
  hpool <- asks App._ssPool
  let bbox = TileLib.getBbox buffer z xy
      tableName = Layer.getLayerSetting layer Layer._layerTableName
      query = layerQueryStreamingWkbProperties config tableName
      action = HasqlCursorQueryTransactions.cursorQuery bbox query
      session = HasqlTransactionSession.transaction HasqlTransactionSession.ReadCommitted HasqlTransactionSession.Read action
  liftIO $ HasqlPool.use hpool session

findSourceFeaturesStreaming :: (MonadIO m, MonadReader App.ServerState m) => TypesConfig.Config -> Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> m (Either HasqlPool.UsageError TypesMvtFeatures.StreamingLayer)
findSourceFeaturesStreaming config layer z xy = do
  buffer <- asks (^. App.ssBuffer)
  hpool <- asks App._ssPool
  let bbox = TileLib.getBbox buffer z xy
      tableName = Layer.getLayerSetting layer Layer._layerTableName
      query = sourceLayerQueryStreamingWkbProperties config tableName
      action = HasqlCursorQueryTransactions.cursorQuery bbox query
      session = HasqlTransactionSession.transaction HasqlTransactionSession.ReadCommitted HasqlTransactionSession.Read action
  liftIO $ HasqlPool.use hpool session

-- Helpers
layerQueryGeoJSON :: Text.Text -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) (Sequence.Seq (Geospatial.GeoFeature AesonTypes.Value))
layerQueryGeoJSON tableName =
  HasqlCursorQuery.cursorQuery sql Tile.bboxEncoder (HasqlCursorQuery.reducingDecoder geoJsonDecoder Layer.foldSeq) HasqlCursorQuery.batchSize_10000
  where
    sql = TextEncoding.encodeUtf8 $ "SELECT geojson FROM " <> tableName <> layerQueryWhereClause

geoJsonDecoder :: HasqlDecoders.Row (Geospatial.GeoFeature AesonTypes.Value)
geoJsonDecoder =
  HasqlDecoders.column $ HasqlDecoders.jsonBytes $ convertDecoder eitherDecode
  where
    eitherDecode = Aeson.eitherDecode :: LazyByteString.ByteString -> Either String (Geospatial.GeoFeature AesonTypes.Value)

sourceLayerQueryStreamingWkbProperties :: TypesConfig.Config -> Text.Text -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) TypesMvtFeatures.StreamingLayer
sourceLayerQueryStreamingWkbProperties config tableName =
  HasqlCursorQuery.cursorQuery sql Tile.bboxEncoder (HasqlCursorQuery.reducingDecoder (wkbPropertiesDecoder config) GeoJsonStreamingToMvt.foldStreamingLayer) HasqlCursorQuery.batchSize_10000
  where
    sql = TextEncoding.encodeUtf8 $ "SELECT ST_AsBinary(row.wkb_geometry), (to_jsonb(row) - 'wkb_geometry') :: JSON FROM (SELECT * FROM "
            <> tableName <> ") row " <> layerQueryWhereClause

layerQueryStreamingWkbProperties :: TypesConfig.Config -> Text.Text -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) TypesMvtFeatures.StreamingLayer
layerQueryStreamingWkbProperties config tableName =
  HasqlCursorQuery.cursorQuery sql Tile.bboxEncoder (HasqlCursorQuery.reducingDecoder (wkbPropertiesDecoder config) GeoJsonStreamingToMvt.foldStreamingLayer) HasqlCursorQuery.batchSize_10000
  where
    sql = TextEncoding.encodeUtf8 $ "SELECT ST_AsBinary(wkb_geometry), properties FROM " <> tableName <> layerQueryWhereClause

wkbPropertiesDecoder :: TypesConfig.Config -> HasqlDecoders.Row (Geospatial.GeospatialGeometry, AesonTypes.Value)
wkbPropertiesDecoder config =
  (\geom props -> (MapnikVectorTile.convertClipSimplify config geom, props))
    <$> HasqlDecoders.column (HasqlDecoders.custom (\_ -> convertDecoder Ewkb.parseByteString))
    <*> HasqlDecoders.column HasqlDecoders.json

convertDecoder :: (LazyByteString.ByteString -> Either String b) -> ByteString.ByteString -> Either Text.Text b
convertDecoder decoder =
  either (Left . Text.pack) Right . decoder . LazyByteString.fromStrict

layerQueryWhereClause :: Text.Text
layerQueryWhereClause =
  " WHERE ST_Intersects(wkb_geometry, ST_Transform(ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1, $2), ST_MakePoint($3, $4)), 3857), 4326));"

checkLayerExists :: (MonadIO m) => HasqlPool.Pool -> Text.Text -> m (Either Text.Text (Maybe Text.Text))
checkLayerExists pool layerTableName =
  DB.runTransaction HasqlTransactionSession.Read pool action
  where
    action = HasqlTransaction.statement layerTableName checkLayerExistsQuery

checkLayerExistsQuery :: HasqlStatement.Statement Text.Text (Maybe Text.Text)
checkLayerExistsQuery =
  HasqlStatement.Statement sql (HasqlEncoders.param HasqlEncoders.text) decoder False
  where
    sql = "SELECT to_regclass($1) :: VARCHAR;"
    decoder = HasqlDecoders.singleRow $ HasqlDecoders.nullableColumn HasqlDecoders.text

getTableColumns :: (MonadIO m) => HasqlPool.Pool -> Text.Text -> m (Either Text.Text (Maybe Text.Text))
getTableColumns pool layerTableName =
  DB.runTransaction HasqlTransactionSession.Read pool action
  where
    action = HasqlTransaction.statement layerTableName getTableColumnsQuery

getTableColumnsQuery :: HasqlStatement.Statement Text.Text (Maybe Text.Text)
getTableColumnsQuery =
  HasqlStatement.Statement sql (HasqlEncoders.param HasqlEncoders.text) decoder False
  where
    sql = "SELECT column_name, udt_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = $1 WHERE udt_name IN blah;"
    decoder = HasqlDecoders.singleRow $ HasqlDecoders.nullableColumn HasqlDecoders.text
