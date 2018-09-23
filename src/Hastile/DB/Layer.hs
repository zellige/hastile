{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Layer where

import qualified Control.Foldl                  as Foldl
import           Control.Lens                   ((^.))
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import qualified Data.Aeson                     as Aeson
import qualified Data.Aeson.Types               as AesonTypes
import qualified Data.ByteString                as ByteString
import qualified Data.ByteString.Lazy           as LazyByteString
import qualified Data.Ewkb                      as Ewkb
import qualified Data.Geometry.Types.Geography  as TypesGeography
import qualified Data.Geospatial                as Geospatial
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as TextEncoding
import qualified Hasql.CursorQuery              as HasqlCursorQuery
import qualified Hasql.CursorQuery.Transactions as HasqlCursorQueryTransactions
import qualified Hasql.Decoders                 as HasqlDecoders
import qualified Hasql.Pool                     as HasqlPool
import qualified Hasql.Transaction.Sessions     as HasqlTransactionSession

import qualified Hastile.Lib.Tile               as TileLib
import qualified Hastile.Types.App              as App
import qualified Hastile.Types.Layer            as Layer
import qualified Hastile.Types.Layer.Format     as LayerFormat
import qualified Hastile.Types.Tile             as Tile

findFeatures :: (MonadIO m, MonadReader App.ServerState m) => Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> m (Either HasqlPool.UsageError [Geospatial.GeoFeature AesonTypes.Value])
findFeatures layer z xy = do
  buffer <- asks (^. App.ssBuffer)
  hpool <- asks App._ssPool
  let bbox = TileLib.getBbox buffer z xy
      query = getLayerQuery layer
      action = HasqlCursorQueryTransactions.cursorQuery bbox query
      session = HasqlTransactionSession.transaction HasqlTransactionSession.ReadCommitted HasqlTransactionSession.Read action
  liftIO $ HasqlPool.use hpool session

getLayerQuery :: Layer.Layer -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) [Geospatial.GeoFeature AesonTypes.Value]
getLayerQuery layer =
  case layerFormat of
    LayerFormat.GeoJSON ->
      layerQueryGeoJSON tableName
    LayerFormat.WkbProperties ->
      layerQueryWkbProperties tableName
  where
    tableName = Layer.getLayerSetting layer Layer._layerTableName
    layerFormat = Layer.getLayerSetting layer Layer._layerFormat

layerQueryGeoJSON :: Text.Text -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) [Geospatial.GeoFeature AesonTypes.Value]
layerQueryGeoJSON tableName =
  HasqlCursorQuery.cursorQuery sql Tile.bboxEncoder (HasqlCursorQuery.reducingDecoder geoJsonDecoder Foldl.list) HasqlCursorQuery.batchSize_10000
  where
    sql = TextEncoding.encodeUtf8 $ "SELECT geojson FROM " <> tableName <> layerQueryWhereClause

layerQueryWkbProperties :: Text.Text -> HasqlCursorQuery.CursorQuery (Tile.BBox Tile.Metres) [Geospatial.GeoFeature AesonTypes.Value]
layerQueryWkbProperties tableName =
  HasqlCursorQuery.cursorQuery sql Tile.bboxEncoder (HasqlCursorQuery.reducingDecoder wkbPropertiesDecoder Foldl.list) HasqlCursorQuery.batchSize_10000
  where
    sql = TextEncoding.encodeUtf8 $ "SELECT ST_AsBinary(wkb_geometry), properties FROM " <> tableName <> layerQueryWhereClause

geoJsonDecoder :: HasqlDecoders.Row (Geospatial.GeoFeature AesonTypes.Value)
geoJsonDecoder =
  HasqlDecoders.column $ HasqlDecoders.jsonBytes $ convertDecoder eitherDecode
  where
    eitherDecode = Aeson.eitherDecode :: LazyByteString.ByteString -> Either String (Geospatial.GeoFeature AesonTypes.Value)

wkbPropertiesDecoder :: HasqlDecoders.Row (Geospatial.GeoFeature AesonTypes.Value)
wkbPropertiesDecoder =
  (\x y -> Geospatial.GeoFeature Nothing x y Nothing)
    <$> HasqlDecoders.column (HasqlDecoders.custom (\_ -> convertDecoder Ewkb.parseByteString))
    <*> HasqlDecoders.column HasqlDecoders.json

convertDecoder :: (LazyByteString.ByteString -> Either String b) -> ByteString.ByteString -> Either Text.Text b
convertDecoder decoder =
  either (Left . Text.pack) Right . decoder . LazyByteString.fromStrict

layerQueryWhereClause :: Text.Text
layerQueryWhereClause =
  " WHERE ST_Intersects(wkb_geometry, ST_Transform(ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1, $2), ST_MakePoint($3, $4)), 3857), 4326));"
