{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Table where

import qualified Control.Exception.Base        as ControlException
import qualified Control.Monad.Except          as Except
import qualified Control.Monad.IO.Class        as MonadIO
import qualified Data.Either                   as DataEither
import qualified Data.Geometry.Types.Geography as GeometryTypesGeography
import qualified Data.Geospatial               as Geospatial
import qualified Data.Map.Strict               as DataMapStrict
import           Data.String.Here.Interpolated
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TextEncoding
import qualified Data.Traversable              as Traversable
import qualified Data.Wkt                      as Wkt
import qualified Hasql.Decoders                as HasqlDecoders
import qualified Hasql.Encoders                as HasqlEncoders
import qualified Hasql.Pool                    as HasqlPool
import qualified Hasql.Statement               as HasqlStatement
import qualified Hasql.Transaction             as HasqlTransaction
import qualified Hasql.Transaction.Sessions    as HasqlTransactionSession
import qualified Katip
import qualified Text.Trifecta.Result          as TrifectaResult

import qualified Hastile.Config                as Config
import qualified Hastile.DB                    as DB
import qualified Hastile.Lib.Log               as LibLog
import qualified Hastile.Types.Config          as Config
import qualified Hastile.Types.Layer           as Layer
import qualified Hastile.Types.Layer.Security  as LayerSecurity

type RunCheckConfig = Katip.LogEnv -> FilePath -> Config.Config -> IO ()

nullCheckConfig :: RunCheckConfig
nullCheckConfig _ _ _ = pure ()

checkConfig :: RunCheckConfig
checkConfig logEnv cfgFile Config.Config{..} = do
  pool <- HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection)
  let layers = uncurry Layer.Layer <$> DataMapStrict.toList _configLayers
  result <- mapM (checkLayerExists pool) layers
  case DataEither.lefts result of
    [] ->
      pure ()
    errs ->
      ControlException.bracket (pure logEnv) (\_ -> pure ()) $ \le ->
        Katip.runKatipContextT le (mempty :: Katip.LogContexts) mempty (LibLog.logErrors cfgFile errs)
  HasqlPool.release pool

writeConfigFromDatabaseTables :: FilePath -> Config.Config -> Except.ExceptT Text.Text IO Config.Config
writeConfigFromDatabaseTables cfgFile config = do
  textLayers <- MonadIO.liftIO (getTables config) >>= Except.liftEither
  boxes <- MonadIO.liftIO (getBboxes config textLayers) >>= Except.liftEither
  let listLayers = zipWith (\b t -> (t, Layer.defaultLayerSettings { Layer._layerBounds = b, Layer._layerSecurity = Just LayerSecurity.Public } )) boxes textLayers
  Config.writeLayers listLayers config cfgFile
  MonadIO.liftIO $ Config.getConfig cfgFile

getTables :: MonadIO.MonadIO m => Config.Config -> m (Either Text.Text [Text.Text])
getTables Config.Config{..} = do
  pool <- MonadIO.liftIO $ HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection)
  errOrList <- wkbGeometryTables pool
  MonadIO.liftIO $ HasqlPool.release pool
  pure errOrList

getBboxes :: Config.Config -> [Text.Text] -> IO (Either Text.Text [Maybe GeometryTypesGeography.BoundingBox])
getBboxes Config.Config{..} layerTableNames = do
  pool <- HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection)
  errOrList <- Traversable.mapM (boxFromTable pool) layerTableNames
  HasqlPool.release pool
  pure (Traversable.sequence errOrList)

checkLayerExists :: MonadIO.MonadIO m => HasqlPool.Pool -> Layer.Layer -> m (Either String ())
checkLayerExists pool layer = do
  let layerTableName = Layer.layerTableName layer
  er <- checkLayerExists' pool layerTableName
  case er of
    Left err      -> pure . Left $ Text.unpack err
    Right Nothing -> pure . Left $ "Could not find table: \'" <> Text.unpack layerTableName <> "\'"
    Right _       -> pure $ Right ()

checkLayerExists' :: (MonadIO.MonadIO m) => HasqlPool.Pool -> Text.Text -> m (Either Text.Text (Maybe Text.Text))
checkLayerExists' pool layerTableName =
  DB.runTransaction HasqlTransactionSession.Read pool action
  where
    action = HasqlTransaction.statement layerTableName checkLayerExistsQuery

checkLayerExistsQuery :: HasqlStatement.Statement Text.Text (Maybe Text.Text)
checkLayerExistsQuery =
  HasqlStatement.Statement sql (HasqlEncoders.param HasqlEncoders.text) decoder False
  where
    sql = [i|
      SELECT to_regclass($1) :: VARCHAR;
    |]
    decoder = HasqlDecoders.singleRow $ HasqlDecoders.nullableColumn HasqlDecoders.text

wkbGeometryTables :: MonadIO.MonadIO m => HasqlPool.Pool -> m (Either Text.Text [Text.Text])
wkbGeometryTables pool =
  DB.runTransaction HasqlTransactionSession.Read pool action
  where
    action = HasqlTransaction.statement () wkbGeometryTablesQuery

wkbGeometryTablesQuery :: HasqlStatement.Statement () [Text.Text]
wkbGeometryTablesQuery =
  HasqlStatement.Statement sql HasqlEncoders.unit decoder False
  where
    sql = [i|
      SELECT
        table_name
      FROM
        information_schema.COLUMNS
      WHERE
        column_name = 'wkb_geometry'
      AND
        udt_name = 'geometry'
    |]
    decoder = HasqlDecoders.rowList $ HasqlDecoders.column HasqlDecoders.text

boxFromTable :: (MonadIO.MonadIO m) => HasqlPool.Pool -> Text.Text -> m (Either Text.Text (Maybe GeometryTypesGeography.BoundingBox))
boxFromTable pool layerTableName =
  DB.runTransaction HasqlTransactionSession.Read pool action
    where
      action = HasqlTransaction.statement layerTableName (boxFromTableQuery layerTableName)

boxFromTableQuery :: Text.Text -> HasqlStatement.Statement Text.Text (Maybe GeometryTypesGeography.BoundingBox)
boxFromTableQuery layerTableName =
  HasqlStatement.Statement sql encoder decoder False
  where
    sql = [i|
      SELECT ST_extent(t.wkb_geometry)::text
      FROM ${layerTableName} as t
    |]
    encoder = HasqlEncoders.param HasqlEncoders.text
    decoder = HasqlDecoders.singleRow bboxDecoder

bboxDecoder :: HasqlDecoders.Row (Maybe GeometryTypesGeography.BoundingBox)
bboxDecoder =  HasqlDecoders.nullableColumn $ HasqlDecoders.custom asGeom
  where
    toPayload = Wkt.parseByteString Wkt.box
    asGeom _ y =
      case toPayload y of
        (TrifectaResult.Success (Geospatial.BoundingBoxWithoutCRSXY (Geospatial.PointXY minX minY) (Geospatial.PointXY maxX maxY))) -> Right (GeometryTypesGeography.BoundingBox minX minY maxX maxY)
        _ -> Left "Failed to get bounds of wkb_geometry column"

