{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module DB where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.ByteString            as BS
import           Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import           Data.Text                  as T
import           Data.Text.Encoding         as TE
import           Data.Time
import           GHC.Conc
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as P
import qualified Hasql.Query                as HQ
import qualified Hasql.Session              as HS
import           Servant
import           STMContainers.Map          as STM

import           Tile
import           Types

defaultTileSize :: Pixels
defaultTileSize = Pixels 2048

findFeatures :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
          => Text -> Coordinates -> m (Either P.UsageError [TileFeature])
findFeatures l zxy = do
  sql <- TE.encodeUtf8 <$> getQuery' l zxy
  let sessTfs = HS.query () (mkStatement sql)
  p <- asks _ssPool
  liftIO $ P.use p sessTfs

getQuery' :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
          => Text -> Coordinates -> m Text
getQuery' l zxy = do
  layer <- getLayer l
  pure . escape bbox4326 . _layerQuery $ layer
  where
    (BBox (Metres llX) (Metres llY) (Metres urX) (Metres urY)) = googleToBBoxM defaultTileSize (_zl zxy) (_xy zxy)
    bbox4326 = T.pack $ "ST_Transform(ST_SetSRID(ST_MakeBox2D(\
                        \ST_MakePoint(" ++ show llX ++ ", " ++ show llY ++ "), \
                        \ST_MakePoint(" ++ show urX ++ ", " ++ show urY ++ ")), 3857), 4326)"

getLastModified :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m) => Text -> m String
getLastModified l = do
  layer <- getLayer l
  let lastModified = _layerLastModified layer
      rfc822Str = formatTime defaultTimeLocale rfc822DateFormat lastModified
      toGMT = T.unpack $ dropEnd 3 (T.pack rfc822Str) <> "GMT"
  pure toGMT

getLayer :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m) => Text -> m Layer
getLayer l = do
  ls <- asks _ssStateLayers
  result <- liftIO $ atomically $ STM.lookup l ls
  case result of
    Just layer -> pure layer
    Nothing -> throwError $ err404 { errBody = "Layer not found :( " }

mkStatement :: BS.ByteString -> HQ.Query () [TileFeature]
mkStatement sql = HQ.statement sql
    HE.unit (HD.rowsList (TileFeature <$> HD.value HD.json <*> propsValue)) False
  where
    propsValue = fmap (fromMaybe "") . M.fromList <$> HD.value (HD.hstore replicateM)

-- Replace any occurrance of the string "!bbox_4326!" in a string with some other string
escape :: Text -> Text -> Text
escape bbox query = T.concat . fmap replace' . T.split (== '!') $ query
  where
    replace' "bbox_4326" = bbox
    replace' t = t
