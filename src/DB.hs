{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module DB where

import Control.Lens ((^.))
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.ByteString            as BS
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
import           STMContainers.Map          as STM

import           Tile
import           Types

data LayerError = LayerNotFound

defaultTileSize :: Pixels
defaultTileSize = Pixels 2048

findFeatures :: (MonadIO m, MonadReader ServerState m)
             => Layer -> Coordinates -> m (Either P.UsageError [TileFeature])
findFeatures layer zxy = do
  sql <- mkQuery layer zxy
  let sessTfs = HS.query () (mkStatement (TE.encodeUtf8 sql))
  p <- asks _ssPool
  errOrResult <- liftIO $ P.use p sessTfs
  pure errOrResult

mkQuery :: (MonadReader ServerState m) => Layer -> Coordinates -> m Text
mkQuery layer zxy =
  do buffer <- asks (^. ssBuffer)
     let zoom = _zl zxy
         bboxM = googleToBBoxM defaultTileSize zoom (_xy zxy)
         (BBox (Metres llX) (Metres llY) (Metres urX) (Metres urY)) =
           addBufferToBBox defaultTileSize buffer zoom bboxM
         bbox4326 = T.pack $ "ST_Transform(ST_SetSRID(ST_MakeBox2D(\
                             \ST_MakePoint(" ++ show llX ++ ", " ++ show llY ++ "), \
                             \ST_MakePoint(" ++ show urX ++ ", " ++ show urY ++ ")), 3857), 4326)"
     pure $ escape bbox4326 . _layerQuery $ layer

getLayer :: (MonadIO m, MonadReader ServerState m) => Text -> m (Either LayerError Layer)
getLayer l = do
  ls <- asks _ssStateLayers
  result <- liftIO . atomically $ STM.lookup l ls
  pure $ case result of
    Nothing -> Left LayerNotFound
    Just layer -> Right layer

mkStatement :: BS.ByteString -> HQ.Query () [TileFeature]
mkStatement sql = HQ.statement sql
    HE.unit (HD.rowsList (TileFeature <$> HD.value HD.json)) False

-- Replace any occurrance of the string "!bbox_4326!" in a string with some other string
escape :: Text -> Text -> Text
escape bbox query = T.concat . fmap replace' . T.split (== '!') $ query
  where
    replace' "bbox_4326" = bbox
    replace' t = t

lastModified :: Layer -> String
lastModified layer = T.unpack $ dropEnd 3 (T.pack rfc822Str) <> "GMT"
       where rfc822Str = formatTime defaultTimeLocale rfc822DateFormat $ _layerLastModified layer
