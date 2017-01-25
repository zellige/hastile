{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Lib
    ( api
    , hastileService
    , ServerState (..)
    ) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.Aeson
import           Data.ByteString            as BS
import           Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import           Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Text.Read             as DTR
import           Data.Time
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as P
import qualified Hasql.Query                as HQ
import qualified Hasql.Session              as HS
import           Servant

import           MapboxVectorTile
import           Tile
import           Types

type LayerName = Capture "layer" Text
type Z = Capture "z" Integer
type X = Capture "x" Integer
type Y = Capture "y" Text
type YI = Capture "y" Integer
type HastileApi =    LayerName :> Z :> X :> YI :> "query" :> Get '[PlainText] Text
                :<|> LayerName :> Z :> X :> Y :> Get '[JSON] (Headers '[Header "Last-Modified" String] GeoJson)
                :<|> LayerName :> Z :> X :> Y :> Get '[OctetStream] (Headers '[Header "Last-Modified" String] ByteString)

api :: Proxy HastileApi
api = Proxy

defaultTileSize :: Pixels
defaultTileSize = Pixels 2048

hastileService :: ServerState -> Server HastileApi
hastileService state =
  enter (runReaderTNat state) (getQuery :<|> getJson :<|> getTile)

parseMvtExt :: Integral a => Text -> Either String (a, Text)
parseMvtExt s = decimal $ T.take (T.length s - T.length ".mvt") s

parseJsonExt :: Integral a => Text -> Either String (a, Text)
parseJsonExt s = decimal $ T.take (T.length s - T.length ".json") s

getQuery :: (MonadError ServantErr m, MonadReader ServerState m)
         => Text -> Integer -> Integer -> Integer -> m Text
getQuery l z x y = getQuery' l (Coordinates (ZoomLevel z) (GoogleTileCoords x y))

getTile :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Integer -> Integer -> Text -> m (Headers '[Header "Last-Modified" String] ByteString)
getTile l z x stringY =
    case parseMvtExt stringY of
      Left e -> fail $ show e
      Right (y, _) -> getTile' l (Coordinates (ZoomLevel z) (GoogleTileCoords x y))

getJson :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Integer -> Integer -> Text -> m (Headers '[Header "Last-Modified" String] GeoJson)
getJson l z x stringY =
    case parseJsonExt stringY of
      Left e -> fail $ show e
      Right (y, _) -> getJson' l (Coordinates (ZoomLevel z) (GoogleTileCoords x y))

getTile' :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Coordinates -> m (Headers '[Header "Last-Modified" String] ByteString)
getTile' l zxy = do
  let tileReturn geoJson' pp' = fromGeoJSON defaultTileSize geoJson' l pp' zxy
  pp <- asks _pluginDir
  errorOrJson <- getJson'' l zxy
  lastModified <- getLastModified l
  eet <- liftIO $ tileReturn errorOrJson pp
  case eet of
    Left e -> pure $ addHeader lastModified $ encodeUtf8 e
    Right tile -> pure $ addHeader lastModified tile


getJson' :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Coordinates -> m (Headers '[Header "Last-Modified" String] GeoJson)
getJson' l zxy = do
  lastModified <- getLastModified l
  errorOrJson <- getJson'' l zxy
  pure $ addHeader lastModified errorOrJson

getJson'' :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
          => Text -> Coordinates -> m GeoJson
getJson'' l zxy = do
  sql <- encodeUtf8 <$> getQuery' l zxy
  let sessTfs = HS.query () (mkStatement sql)
  p <- asks _pool
  tfsOrError <- liftIO $ P.use p sessTfs
  case tfsOrError of
    Left e -> fail $ show e
    Right tfs -> pure $ mkGeoJSON tfs

getQuery' :: (MonadError ServantErr m, MonadReader ServerState m)
         => Text -> Coordinates -> m Text
getQuery' l zxy = do
  layer <- getLayer l
  pure . escape bbox4326 . _layerQuery $ layer
  where (BBox (Metres llX) (Metres llY) (Metres urX) (Metres urY)) = googleToBBoxM defaultTileSize (_zl zxy) (_xy zxy)
        bbox4326 = T.pack $ "ST_Transform(ST_SetSRID(ST_MakeBox2D(\
                            \ST_MakePoint(" ++ show llX ++ ", " ++ show llY ++ "), \
                            \ST_MakePoint(" ++ show urX ++ ", " ++ show urY ++ ")), 3857), 4326)"

mkGeoJSON :: [TileFeature] -> GeoJson
mkGeoJSON tfs = M.fromList [ ("type", String "FeatureCollection")
                             , ("features", toJSON . fmap mkFeature $ tfs)
                             ]

mkFeature :: TileFeature -> Value
mkFeature tf = toJSON featureMap
  where featureMap = M.fromList [ ("type", String "Feature")
                                , ("geometry", _geometry tf)
                                , ("properties", toJSON . _properties $ tf)
                                ] :: M.Map Text Value

mkStatement :: ByteString -> HQ.Query () [TileFeature]
mkStatement sql =
  HQ.statement sql HE.unit
               (HD.rowsList (TileFeature <$> HD.value HD.json <*> propsValue))
               False
  where propsValue = fmap (fromMaybe "") . M.fromList <$> HD.value (HD.hstore replicateM)

getLastModified :: (MonadError ServantErr m, MonadReader ServerState m) => Text -> m String
getLastModified l = do
  layer <- getLayer l
  let lastModified = _layerLastModified layer
      rfc822Str = formatTime defaultTimeLocale rfc822DateFormat lastModified
      toGMT = T.unpack $ dropEnd 3 (T.pack rfc822Str) <> "GMT"
  pure toGMT

getLayer :: (MonadError ServantErr m, MonadReader ServerState m) => Text -> m Layer
getLayer l = do
  ls <- asks _stateLayers
  case M.lookup l ls of
    Just layer -> pure layer
    Nothing -> throwError $ err404 { errBody = "Layer not found :( " }

-- Replace any occurrance of the string "!bbox_4326!" in a string with some other string
escape :: Text -> Text -> Text
escape bbox query = T.concat . fmap replace' . T.split (== '!') $ query
  where
    replace' "bbox_4326" = bbox
    replace' t = t
