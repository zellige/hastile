{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Layer where

import qualified Control.Foldl                       as Foldl
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
import qualified Hasql.Pool                          as HasqlPool
import qualified Hasql.Transaction.Sessions          as HasqlTransactionSession

import qualified Hastile.Lib.Tile                    as TileLib
import qualified Hastile.Types.App                   as App
import qualified Hastile.Types.Layer                 as Layer
import qualified Hastile.Types.Tile                  as Tile

-- API
findSourceFeaturesStreaming :: (MonadIO m, MonadReader App.ServerState m) => TypesConfig.Config -> Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> m (Either HasqlPool.UsageError TypesMvtFeatures.StreamingLayer)
findSourceFeaturesStreaming config layer z xy =
  findFeaturesStreaming z xy query
  where query = layerQueryStreamingSource config layer

findWkbPropertiesFeaturesStreaming :: (MonadIO m, MonadReader App.ServerState m) => TypesConfig.Config -> Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> m (Either HasqlPool.UsageError TypesMvtFeatures.StreamingLayer)
findWkbPropertiesFeaturesStreaming config layer z xy =
  findFeaturesStreaming z xy query
  where query = layerQueryStreamingWkbProperties config layer

findFeatures :: (MonadIO m, MonadReader App.ServerState m) => Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> m (Either HasqlPool.UsageError (Sequence.Seq (Geospatial.GeoFeature AesonTypes.Value)))
findFeatures layer z xy = do
  buffer <- asks (^. App.ssBuffer)
  hpool <- asks App._ssPool
  let bbox = TileLib.getBbox buffer z xy
      query = layerQueryGeoJSON layer
      action = HasqlCursorQueryTransactions.cursorQuery bbox query
      session = HasqlTransactionSession.transaction HasqlTransactionSession.ReadCommitted HasqlTransactionSession.Read action
  liftIO $ HasqlPool.use hpool session

-- Helpers
findFeaturesStreaming :: (MonadIO m, MonadReader App.ServerState m) => TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) TypesMvtFeatures.StreamingLayer -> m (Either HasqlPool.UsageError TypesMvtFeatures.StreamingLayer)
findFeaturesStreaming z xy query = do
  buffer <- asks (^. App.ssBuffer)
  hpool <- asks App._ssPool
  let bbox = TileLib.getBbox buffer z xy
      action = HasqlCursorQueryTransactions.cursorQuery bbox query
      session = HasqlTransactionSession.transaction HasqlTransactionSession.ReadCommitted HasqlTransactionSession.Read action
  liftIO $ HasqlPool.use hpool session

layerQueryGeoJSON :: Layer.Layer -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) (Sequence.Seq (Geospatial.GeoFeature AesonTypes.Value))
layerQueryGeoJSON layer =
  HasqlCursorQuery.cursorQuery sql Tile.bboxEncoder (HasqlCursorQuery.reducingDecoder geoJsonDecoder foldSeq) HasqlCursorQuery.batchSize_10000
  where
    sql = TextEncoding.encodeUtf8 $ "SELECT geojson FROM " <> Layer.layerTableName layer <> layerQueryWhereClause (Layer.layerGeomColumn layer) (Layer.layerSrid layer)

geoJsonDecoder :: HasqlDecoders.Row (Geospatial.GeoFeature AesonTypes.Value)
geoJsonDecoder =
  HasqlDecoders.column $ HasqlDecoders.jsonBytes $ convertDecoder eitherDecode
  where
    eitherDecode = Aeson.eitherDecode :: LazyByteString.ByteString -> Either String (Geospatial.GeoFeature AesonTypes.Value)

layerQueryStreamingSource :: TypesConfig.Config -> Layer.Layer -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) TypesMvtFeatures.StreamingLayer
layerQueryStreamingSource config layer =
  layerQueryStreaming config sql
  where sql = TextEncoding.encodeUtf8 $ "SELECT ST_AsBinary(row." <> Layer.layerGeomColumn layer <> "), (to_jsonb(row) - '" <> Layer.layerGeomColumn layer <> "') :: JSON FROM (SELECT * FROM "
                <> Layer.layerTableName layer <> ") row " <> layerQueryWhereClause (Layer.layerGeomColumn layer) (Layer.layerSrid layer)

layerQueryStreamingWkbProperties :: TypesConfig.Config -> Layer.Layer -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) TypesMvtFeatures.StreamingLayer
layerQueryStreamingWkbProperties config layer =
  layerQueryStreaming config sql
  where sql = TextEncoding.encodeUtf8 $ "SELECT ST_AsBinary(" <> Layer.layerGeomColumn layer <> "), properties FROM " <> Layer.layerTableName layer <> layerQueryWhereClause (Layer.layerGeomColumn layer) (Layer.layerSrid layer)

layerQueryStreaming :: TypesConfig.Config -> ByteString.ByteString -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) TypesMvtFeatures.StreamingLayer
layerQueryStreaming config sql =
  HasqlCursorQuery.cursorQuery sql Tile.bboxEncoder (HasqlCursorQuery.reducingDecoder (wkbPropertiesDecoder config) GeoJsonStreamingToMvt.foldStreamingLayer) HasqlCursorQuery.batchSize_10000

wkbPropertiesDecoder :: TypesConfig.Config -> HasqlDecoders.Row (Geospatial.GeospatialGeometry, AesonTypes.Value)
wkbPropertiesDecoder config =
  (\geom props -> (MapnikVectorTile.convertClipSimplify config geom, props))
    <$> HasqlDecoders.column (HasqlDecoders.custom (\_ -> convertDecoder Ewkb.parseByteString))
    <*> HasqlDecoders.column HasqlDecoders.json

convertDecoder :: (LazyByteString.ByteString -> Either String b) -> ByteString.ByteString -> Either Text.Text b
convertDecoder decoder =
  either (Left . Text.pack) Right . decoder . LazyByteString.fromStrict

layerQueryWhereClause :: Text.Text -> Integer -> Text.Text
layerQueryWhereClause geometryColumnName srid =
  " WHERE ST_Intersects(" <> geometryColumnName <> ", ST_Transform(ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1, $2), ST_MakePoint($3, $4)), 3857), " <> Text.pack (show srid) <> "));"

foldSeq :: Foldl.Fold a (Sequence.Seq a)
foldSeq = Foldl.Fold step begin done
  where
    begin = Sequence.empty
    step x a = x <> Sequence.singleton a
    done = id
