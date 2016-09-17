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
import           Data.ByteString.Lazy (toStrict)
import           Data.Map                   as M
import           Data.Maybe (fromMaybe)
import           Data.Text                  as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as P
import qualified Hasql.Query                as HQ
import qualified Hasql.Session              as HS
import           Servant

import           SphericalMercator
import           MapboxVectorTile

type Layer = Capture "layer" Text
type Z     = Capture "z" Integer
type X     = Capture "x" Integer
type Y     = Capture "y" Integer
type HastileApi =    Layer :> Z :> X :> Y :> "query" :> Get '[PlainText] Text
                :<|> Layer :> Z :> X :> Y :> "mvt" :> Get '[OctetStream] ByteString

-- TODO: make lenses!
data ServerState = ServerState { pool :: P.Pool
                               , stateLayers :: Map Text Text
                               }

data TileFeatures = TileFeatures { geoJson :: BS.ByteString
                                 , properties :: Map Text Text
                                 } deriving (Show)

api :: Proxy HastileApi
api = Proxy

hastileService :: ServerState -> Server HastileApi
hastileService state =
  enter (runReaderTNat state) (getQuery :<|> getTile)


getQuery :: (MonadError ServantErr m, MonadReader ServerState m)
         => Text -> Integer -> Integer -> Integer -> m Text
getQuery l z x y = do
  s <- ask
  let ls = stateLayers s
  case M.lookup l ls of
    Just rawQuery -> pure . escape bbox4326 $ rawQuery
    Nothing -> throwError $ err404 { errBody = "Layer not found :(" }
  where (BBox (Metres llX) (Metres llY) (Metres urX) (Metres urY)) =
          googleToBBoxM 256 (ZoomLevel z) (GoogleTileCoords x y)
        bbox4326 = T.pack $ ("ST_Transform(ST_SetSRID(ST_MakeBox2D(\
                            \ST_MakePoint(" ++ show llX ++ ", " ++ show llY ++ ", \
                            \ST_MakePoint(" ++ show urX ++ ", " ++ show urY ++ ")) 3857) 4326)")

getTile :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Integer -> Integer -> Integer -> m ByteString
getTile l z x y = do
  sql <- encodeUtf8 <$> getQuery l z x y
  s <- ask
  let p       = pool s
      sessTfs = HS.query () (mkStatement sql)
  tfsM <- liftIO $ P.use p sessTfs
  case tfsM of
    Left e -> fail $ show e
    Right tfs -> liftIO $ fromGeoJSON 256
                                      (mkGeoJSON tfs)
                                      l
                                      "/usr/local/lib/mapnik/input"
                                      (ZoomLevel z)
                                      (GoogleTileCoords x y)
  return sql

mkGeoJSON :: [TileFeatures] -> ByteString
mkGeoJSON = undefined

mkStatement :: ByteString -> HQ.Query () [TileFeatures]
mkStatement sql =
  HQ.statement sql HE.unit
               (HD.rowsList (TileFeatures <$> jsonValue <*> propsValue))
               False
  where jsonValue = toStrict . encode <$> HD.value HD.json
        -- jsonValue = HD.value (HD.jsonBytes (bimap T.pack id . eitherDecode . fromStrict))
        propsValue = fmap (fromMaybe "") . M.fromList <$> (HD.value $ HD.hstore replicateM)

-- Replace any occurrance of the string "!bbox_4326!" in a string with some other string
escape :: Text -> Text -> Text
escape bbox query = T.concat . fmap replace' . T.split (== '!') $ query
  where replace' "bbox_4326" = bbox
        replace' t = t
