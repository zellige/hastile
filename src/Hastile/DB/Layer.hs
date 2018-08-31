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
import qualified Data.ByteString.Lazy          as LazyByteString
import qualified Data.Geometry.Types.Geography as DGTT
import qualified Data.Geospatial               as Geospatial
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TextEncoding
import qualified Data.Wkb                      as Wkb
import qualified Hasql.Decoders                as HD
import qualified Hasql.Pool                    as Pool
import qualified Hasql.Query                   as HQ
import qualified Hasql.Transaction             as Transaction
import qualified Hasql.Transaction.Sessions    as Transaction

import qualified Hastile.Lib.Tile              as TileLib
import qualified Hastile.Types.App             as App
import qualified Hastile.Types.Layer           as Layer
import qualified Hastile.Types.Layer.Format    as LayerFormat
import qualified Hastile.Types.Tile            as Tile

findFeatures :: (MonadIO m, MonadReader App.ServerState m) => Layer.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> m (Either Pool.UsageError [Geospatial.GeoFeature AesonTypes.Value])
findFeatures layer z xy = do
  buffer <- asks (^. App.ssBuffer)
  let
    bbox = TileLib.getBbox buffer z xy
    query = getLayerQuery layer
    action = Transaction.query bbox query
    session = Transaction.transaction
                Transaction.ReadCommitted Transaction.Read action
  hpool <- asks App._ssPool
  liftIO $ Pool.use hpool session

getLayerQuery :: Layer.Layer -> HQ.Query (Tile.BBox Tile.Metres) [Geospatial.GeoFeature AesonTypes.Value]
getLayerQuery layer =
  case layerFormat of
    LayerFormat.GeoJSON ->
      layerQueryGeoJSON tableName
    LayerFormat.WkbProperties ->
      layerQueryWkbProperties tableName
  where
    tableName = Layer.getLayerSetting layer Layer._layerTableName
    layerFormat = Layer.getLayerSetting layer Layer._layerFormat

layerQueryGeoJSON :: Text.Text -> HQ.Query (Tile.BBox Tile.Metres) [Geospatial.GeoFeature AesonTypes.Value]
layerQueryGeoJSON tableName =
    HQ.statement sql Tile.bboxEncoder (HD.rowsList jsonDecoder) False
  where
    sql = TextEncoding.encodeUtf8 $ Text.pack $
            "SELECT geojson FROM " ++
            Text.unpack tableName ++ layerQueryWhereClause

layerQueryWkbProperties :: Text.Text -> HQ.Query (Tile.BBox Tile.Metres) [Geospatial.GeoFeature AesonTypes.Value]
layerQueryWkbProperties tableName =
    HQ.statement sql Tile.bboxEncoder (HD.rowsList wkbPropertiesDecoder) False
  where
    sql = TextEncoding.encodeUtf8 $ Text.pack $
            "SELECT ST_AsBinary(wkb_geometry), properties FROM " ++
            Text.unpack tableName ++ layerQueryWhereClause

jsonDecoder :: HD.Row (Geospatial.GeoFeature AesonTypes.Value)
jsonDecoder =
  HD.value $
    HD.jsonBytes $
      either
        (Left . Text.pack)
        Right . eitherDecode . LazyByteString.fromStrict
  where
    eitherDecode =
      Aeson.eitherDecode :: LazyByteString.ByteString
        -> Either String (Geospatial.GeoFeature AesonTypes.Value)

wkbPropertiesDecoder :: HD.Row (Geospatial.GeoFeature AesonTypes.Value)
wkbPropertiesDecoder =
  (\x y -> Geospatial.GeoFeature Nothing x y Nothing)
    <$> HD.value (HD.custom (\_ -> either (Left . Text.pack) Right . Wkb.parseByteString . LazyByteString.fromStrict))
    <*> HD.value HD.json

layerQueryWhereClause :: String
layerQueryWhereClause =
  " WHERE ST_Intersects(wkb_geometry, ST_Transform(ST_SetSRID(ST_MakeBox2D(ST_MakePoint($1, $2), ST_MakePoint($3, $4)), 3857), 4326));"
