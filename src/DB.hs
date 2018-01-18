{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module DB where

import           Control.Lens               ((^.))
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.Aeson
import           Data.ByteString            as BS
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Encoding         as TE
import           Data.Time
import           GHC.Conc
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as P
import qualified Hasql.Query                as HQ
import qualified Hasql.Session              as HS
import           STMContainers.Map          as STM

import qualified Data.Geometry.Types.Types  as DGTT

import           Tile
import qualified Types                      as T

data LayerError = LayerNotFound

findFeatures :: (MonadIO m, MonadReader T.ServerState m)
             => T.Layer -> DGTT.Pixels -> (DGTT.Pixels, DGTT.Pixels) -> m (Either P.UsageError [Value])
findFeatures layer z xy = do
  sql <- mkQuery layer z xy
  let sessTfs = HS.query () (mkStatement (TE.encodeUtf8 sql))
  p <- asks T._ssPool
  errOrResult <- liftIO $ P.use p sessTfs
  pure errOrResult

mkQuery :: (MonadReader T.ServerState m) => T.Layer -> T.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> m T.Text
mkQuery layer z xy =
  do buffer <- asks (^. T.ssBuffer)
     let bboxM = googleToBBoxM T.defaultTileSize z xy
         (BBox (Metres llX) (Metres llY) (Metres urX) (Metres urY)) =
           addBufferToBBox T.defaultTileSize buffer z bboxM
         bbox4326 = T.pack $ "ST_Transform(ST_SetSRID(ST_MakeBox2D(\
                             \ST_MakePoint(" ++ show llX ++ ", " ++ show llY ++ "), \
                             \ST_MakePoint(" ++ show urX ++ ", " ++ show urY ++ ")), 3857), 4326)"
     pure $ escape bbox4326 . T._layerQuery $ layer

getLayer :: (MonadIO m, MonadReader T.ServerState m) => T.Text -> m (Either LayerError T.Layer)
getLayer l = do
  ls <- asks T._ssStateLayers
  result <- liftIO . atomically $ STM.lookup l ls
  pure $ case result of
    Nothing    -> Left LayerNotFound
    Just layer -> Right layer

mkStatement :: BS.ByteString -> HQ.Query () [Value]
mkStatement sql = HQ.statement sql
    HE.unit (HD.rowsList (HD.value HD.json)) False

-- Replace any occurrance of the string "!bbox_4326!" in a string with some other string
escape :: T.Text -> T.Text -> T.Text
escape bbox = T.concat . fmap replace' . T.split (== '!')
  where
    replace' "bbox_4326" = bbox
    replace' t           = t

lastModified :: T.Layer -> String
lastModified layer = T.unpack $ T.dropEnd 3 (T.pack rfc822Str) <> "GMT"
       where rfc822Str = formatTime defaultTimeLocale rfc822DateFormat $ T._layerLastModified layer
