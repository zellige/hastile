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
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Lazy          as LazyByteString
import qualified Data.Geometry.Types.Geography as TypesGeography
import qualified Data.Geospatial               as Geospatial
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TextEncoding
import qualified Data.Wkb                      as Wkb
import qualified Hasql.Decoders                as HasqlDecoders
import qualified Hasql.Pool                    as HasqlPool
import qualified Hasql.Query                   as HasqlQuery
import qualified Hasql.Transaction             as Transaction
import qualified Hasql.Transaction.Sessions    as Transaction

import qualified Hastile.Lib.Tile              as TileLib
import qualified Hastile.Types.App             as App
import qualified Hastile.Types.Layer           as Layer
import qualified Hastile.Types.Layer.Format    as LayerFormat
import qualified Hastile.Types.Tile            as Tile

findFeatures :: (MonadIO m, MonadReader App.ServerState m) => Layer.Layer -> TypesGeography.ZoomLevel -> (TypesGeography.Pixels, TypesGeography.Pixels) -> m (Either HasqlPool.UsageError [Geospatial.GeoFeature AesonTypes.Value])
findFeatures layer z xy = do
  buffer <- asks (^. App.ssBuffer)
  hpool <- asks App._ssPool
  let bbox = TileLib.getBbox buffer z xy
      query = getLayerQuery layer
      action = Transaction.query bbox query
      session = Transaction.transaction Transaction.ReadCommitted Transaction.Read action
  liftIO $ HasqlPool.use hpool session

getLayerQuery :: Layer.Layer -> HasqlQuery.Query (Tile.BBox Tile.Metres) [Geospatial.GeoFeature AesonTypes.Value]
getLayerQuery layer =
  case layerFormat of
    LayerFormat.GeoJSON ->
      layerQueryGeoJSON tableName
    LayerFormat.WkbProperties ->
      layerQueryWkbProperties tableName
  where
    tableName = Layer.getLayerSetting layer Layer._layerTableName
    layerFormat = Layer.getLayerSetting layer Layer._layerFormat

layerQueryGeoJSON :: Text.Text -> HasqlQuery.Query (Tile.BBox Tile.Metres) [Geospatial.GeoFeature AesonTypes.Value]
layerQueryGeoJSON tableName =
    HasqlQuery.statement sql Tile.bboxEncoder (HasqlDecoders.rowsList geoJsonDecoder) False
  where
    sql = TextEncoding.encodeUtf8 . Text.pack $
            "SELECT geojson FROM " ++
            Text.unpack tableName ++ layerQueryWhereClause

layerQueryWkbProperties :: Text.Text -> HasqlQuery.Query (Tile.BBox Tile.Metres) [Geospatial.GeoFeature AesonTypes.Value]
layerQueryWkbProperties tableName =
    HasqlQuery.statement sql Tile.bboxEncoder (HasqlDecoders.rowsList wkbPropertiesDecoder) False
  where
    sql = TextEncoding.encodeUtf8 . Text.pack $
            "SELECT ST_AsBinary(wkb_geometry), properties FROM " ++
            Text.unpack tableName ++ layerQueryWhereClause

geoJsonDecoder :: HasqlDecoders.Row (Geospatial.GeoFeature AesonTypes.Value)
geoJsonDecoder =
  HasqlDecoders.value $ HasqlDecoders.jsonBytes $ convertDecoder eitherDecode
  where
    eitherDecode =
      Aeson.eitherDecode :: LazyByteString.ByteString
        -> Either String (Geospatial.GeoFeature AesonTypes.Value)

wkbPropertiesDecoder :: HasqlDecoders.Row (Geospatial.GeoFeature AesonTypes.Value)
wkbPropertiesDecoder =
  (\x y -> Geospatial.GeoFeature Nothing x y Nothing)
    <$> HasqlDecoders.value (HasqlDecoders.custom (\_ -> convertDecoder Wkb.parseByteString))
    <*> HasqlDecoders.value HasqlDecoders.json

convertDecoder :: (LazyByteString.ByteString -> Either String b) -> ByteString.ByteString -> Either Text.Text b
convertDecoder decoder =
  either (Left . Text.pack) Right . decoder . LazyByteString.fromStrict

layerQueryWhereClause :: String
layerQueryWhereClause =
  " WHERE ST_Intersects(wkb_geometry, ST_Transform(ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1, $2), ST_MakePoint($3, $4)), 3857), 4326));"
