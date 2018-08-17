{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB where

import           Control.Lens               ((^.))
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSChar8
import qualified Data.Geometry.Types.Types  as DGTT
import qualified Data.Int                   as Int
import           Data.Monoid
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as TE
import qualified Data.Time                  as DT
import           GHC.Conc
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as P
import qualified Hasql.Query                as HQ
import qualified Hasql.Session              as HS
import           STMContainers.Map          as STM

import           Hastile.Tile
import qualified Hastile.Types.App          as App
import qualified Hastile.Types.Config       as Config
import qualified Hastile.Types.Layer        as Layer
import qualified Hastile.Types.Token        as Token

data LayerError = LayerNotFound

findFeatures :: (MonadIO m, MonadReader App.ServerState m)
             => Layer.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> m (Either P.UsageError [Aeson.Value])
findFeatures layer z xy = do
  sql <- mkQuery layer z xy
  let sessTfs = HS.query () (mkStatement (TE.encodeUtf8 sql))
  p <- asks App._ssPool
  liftIO $ P.use p sessTfs

mkQuery :: (MonadReader App.ServerState m) => Layer.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> m Text.Text
mkQuery layer z xy =
  do buffer <- asks (^. App.ssBuffer)
     let bboxM = googleToBBoxM Config.defaultTileSize z xy
         (BBox (Metres llX) (Metres llY) (Metres urX) (Metres urY)) =
           addBufferToBBox Config.defaultTileSize buffer z bboxM
         bbox4326 = Text.pack $ "ST_Transform(ST_SetSRID(ST_MakeBox2D(\
                             \ST_MakePoint(" ++ show llX ++ ", " ++ show llY ++ "), \
                             \ST_MakePoint(" ++ show urX ++ ", " ++ show urY ++ ")), 3857), 4326)"
     pure $ escape bbox4326 . Layer._layerQuery $ layer

getLayer :: (MonadIO m, MonadReader App.ServerState m) => Text.Text -> m (Either LayerError Layer.Layer)
getLayer l = do
  ls <- asks App._ssStateLayers
  result <- liftIO . atomically $ STM.lookup l ls
  pure $ case result of
    Nothing    -> Left LayerNotFound
    Just layer -> Right layer

mkStatement :: BS.ByteString -> HQ.Query () [Aeson.Value]
mkStatement sql = HQ.statement sql
    HE.unit (HD.rowsList (HD.value HD.json)) False

-- Replace any occurrance of the string "!bbox_4326!" in a string with some other string
escape :: Text.Text -> Text.Text -> Text.Text
escape bbox = Text.concat . fmap replace' . Text.split (== '!')
  where
    replace' "bbox_4326" = bbox
    replace' t           = t

lastModified :: Layer.Layer -> Text.Text
lastModified layer = Text.dropEnd 3 (Text.pack rfc822Str) <> "GMT"
       where rfc822Str = DT.formatTime DT.defaultTimeLocale DT.rfc822DateFormat $ Layer._layerLastModified layer

parseIfModifiedSince :: Text.Text -> Maybe DT.UTCTime
parseIfModifiedSince t = DT.parseTimeM True DT.defaultTimeLocale "%a, %e %b %Y %T GMT" $ Text.unpack t

isModifiedTime :: Layer.Layer -> Maybe DT.UTCTime -> Bool
isModifiedTime layer mTime =
  case mTime of
    Nothing   -> True
    Just time -> Layer._layerLastModified layer > time

isModified :: Layer.Layer -> Maybe Text.Text -> Bool
isModified layer mText =
  case mText of
    Nothing   -> True
    Just text -> isModifiedTime layer $ parseIfModifiedSince text

getTokensQuery :: HQ.Query () [Token.TokenLayers]
getTokensQuery =
    HQ.statement sql HE.unit (HD.rowsList Token.tokenDecoder) False
  where
    sql = "SELECT token, layers FROM tokens;"

getTokens :: MonadIO m => String -> P.Pool -> m (Either Text.Text [Token.TokenLayers])
getTokens schemaName pool =
    runDBeither pool action
  where
    action =
      schemaSession schemaName >>
      HS.query () getTokensQuery

getTokenQuery :: HQ.Query Text.Text Token.TokenLayers
getTokenQuery =
    HQ.statement sql (HE.value HE.text) (HD.singleRow Token.tokenDecoder) False
  where
    sql = "SELECT token, layers FROM tokens WHERE token LIKE $1;"

getToken :: MonadIO m => String -> P.Pool -> Text.Text -> m (Either Text.Text Token.TokenLayers)
getToken schemaName pool token =
  runDBeither pool action
  where
    action =
      schemaSession schemaName >>
      HS.query token getTokenQuery

updateOrInsertTokenQuery :: HQ.Query Token.TokenLayers ()
updateOrInsertTokenQuery =
    HQ.statement sql Token.tokenEncoder HD.unit False
  where
    sql = "INSERT INTO tokens (token, layers) VALUES ($1, $2) ON CONFLICT (token) DO UPDATE SET layers = $2;"

updateOrInsertToken :: MonadIO m => String -> P.Pool -> Token.TokenLayers -> m (Either Text.Text ())
updateOrInsertToken schemaName pool tokenLayers =
  runDBeither pool action
  where
    action =
      schemaSession schemaName >>
      HS.query tokenLayers updateOrInsertTokenQuery

deleteTokenQuery :: HQ.Query Text.Text Int.Int64
deleteTokenQuery =
    HQ.statement sql (HE.value HE.text) HD.rowsAffected False
  where
    sql = "DELETE FROM tokens WHERE token LIKE $1;"

deleteToken :: MonadIO m => String -> P.Pool -> Text.Text -> m (Either Text.Text Int.Int64)
deleteToken schemaName pool token =
  runDBeither pool action
  where
    action =
      schemaSession schemaName >>
      HS.query token deleteTokenQuery

runDBeither :: (MonadIO m) => P.Pool -> HS.Session b -> m (Either Text.Text b)
runDBeither hpool action = do
  p <- liftIO $ P.use hpool action
  case p of
    Left e  -> pure . Left  $ Text.pack (show e)
    Right r -> pure . Right $ r

mkSession :: a -> HQ.Query a b -> HS.Session b
mkSession = HS.query

emptySession :: BSChar8.ByteString -> HS.Session ()
emptySession sql = mkSession () $ HQ.statement sql HE.unit HD.unit False

withSchema :: String -> BSChar8.ByteString
withSchema schemaName = "SET search_path TO " <> BSChar8.pack schemaName <> ";"

schemaSession :: String -> HS.Session ()
schemaSession schemaName = emptySession (withSchema schemaName)
