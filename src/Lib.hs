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
import           Data.ByteString.Lazy       (toStrict)
import           Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Text.Read             as DTR
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as P
import qualified Hasql.Query                as HQ
import qualified Hasql.Session              as HS
import           Servant

import           MapboxVectorTile
import           Tile

type Layer = Capture "layer" Text
type Z     = Capture "z" Integer
type X     = Capture "x" Integer
type YI    = Capture "y" Integer
type Y     = Capture "y" Text
type HastileApi =    Layer :> Z :> X :> YI :> "query" :> Get '[PlainText] Text
                :<|> Layer :> Z :> X :> YI :> "geojson" :> Get '[PlainText] Text
                :<|> Layer :> Z :> X :> Y :> Get '[OctetStream] ByteString

-- TODO: make lenses!
data ServerState = ServerState { pool        :: P.Pool
                               , stateLayers :: Map Text Text
                               }

data TileFeature = TileFeature { geometry   :: Value
                               , properties :: Map Text Text
                               }

api :: Proxy HastileApi
api = Proxy

defaultTileSize :: Pixels
defaultTileSize = Pixels 2048

hastileService :: ServerState -> Server HastileApi
hastileService state =
  enter (runReaderTNat state) (getQuery :<|> getJson :<|> getSTile)

getSTile :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Integer -> Integer -> Text -> m ByteString
getSTile l z x stringY =
    case parseY of
      Left e -> fail $ show e
      Right (y, _) -> getTile l z x y
    where
      parseY = decimal $ T.take (T.length stringY - 4) stringY

getTile :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Integer -> Integer -> Integer -> m ByteString
getTile l z x y = do
  geoJson <- getJson' l z x y
  eet <- liftIO $ tileReturn geoJson
  case eet of
    Left e -> pure $ encodeUtf8 e
    Right tile -> pure tile
    where tileReturn geoJson' = fromGeoJSON defaultTileSize
                                  geoJson'
                                  l
                                  "/usr/lib/mapnik/3.0/input"
                                  (ZoomLevel z)
                                  (GoogleTileCoords x y)

getJson :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Integer -> Integer -> Integer -> m Text
getJson l z x y = decodeUtf8 <$> getJson' l z x y

getJson' :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Integer -> Integer -> Integer -> m ByteString
getJson' l z x y = do
  sql <- encodeUtf8 <$> getQuery l z x y
  s <- ask
  let p = pool s
      sessTfs = HS.query () (mkStatement sql)
  tfsM <- liftIO $ P.use p sessTfs
  case tfsM of
    Left e -> fail $ show e
    Right tfs -> pure . mkGeoJSON $ tfs

mkGeoJSON :: [TileFeature] -> ByteString
mkGeoJSON tfs = toStrict . encode $ geoJSON
  where geoJSON = M.fromList [ ("type", String "FeatureCollection")
                             , ("features", toJSON . fmap mkFeature $ tfs)
                             ] :: M.Map Text Value

mkFeature :: TileFeature -> Value
mkFeature tf = toJSON featureMap
  where featureMap = M.fromList [ ("type", String "Feature")
                                , ("geometry", geometry tf)
                                , ("properties", toJSON . properties $ tf)
                                ] :: M.Map Text Value

mkStatement :: ByteString -> HQ.Query () [TileFeature]
mkStatement sql =
  HQ.statement sql HE.unit
               (HD.rowsList (TileFeature <$> HD.value HD.json <*> propsValue))
               False
  where -- jsonValue = toStrict . encode <$> HD.value HD.json
        propsValue = fmap (fromMaybe "") . M.fromList <$> HD.value (HD.hstore replicateM)


getQuery :: (MonadError ServantErr m, MonadReader ServerState m)
         => Text -> Integer -> Integer -> Integer -> m Text
getQuery l z x y = do
  s <- ask
  let ls = stateLayers s
  case M.lookup l ls of
    Just rawQuery -> pure . escape bbox4326 $ rawQuery
    Nothing -> throwError $ err404 { errBody = "Layer not found :(" }
  where (BBox (Metres llX) (Metres llY) (Metres urX) (Metres urY)) =
          googleToBBoxM defaultTileSize (ZoomLevel z) (GoogleTileCoords x y)
        bbox4326 = T.pack $ "ST_Transform(ST_SetSRID(ST_MakeBox2D(\
                            \ST_MakePoint(" ++ show llX ++ ", " ++ show llY ++ "), \
                            \ST_MakePoint(" ++ show urX ++ ", " ++ show urY ++ ")), 3857), 4326)"

-- Replace any occurrance of the string "!bbox_4326!" in a string with some other string
escape :: Text -> Text -> Text
escape bbox query = T.concat . fmap replace' . T.split (== '!') $ query
  where replace' "bbox_4326" = bbox
        replace' t = t
