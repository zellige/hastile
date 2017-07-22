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

import           Control.Lens                    ((^.))
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString                 as BS
import           Data.ByteString.Char8           as BS8
import           Data.ByteString.Lazy.Char8      as LBS
import           Data.Char
import qualified Data.Geography.GeoJSON          as GJ
import qualified Data.Geometry.MapnikVectorTile  as DGM
import qualified Data.Geometry.Types             as DGT
import           Data.Map                        as M
import           Data.Monoid
import           Data.Text                       as T
import           Data.Text.Encoding              as TE
import           Data.Text.Read                  as DTR
import           Data.Time
import qualified Geography.VectorTile            as VT
import qualified Geography.VectorTile.VectorTile as VVT
import           GHC.Conc
import           ListT
import           Network.HTTP.Types.Header       (hLastModified)
import           Servant
import           STMContainers.Map               as STM

import           DB
import           Routes
import           Tile
import           Types

hastileService :: ServerState -> Server HastileApi
hastileService state =
  enter (runReaderTNat state) (returnConfiguration :<|> provisionLayer :<|> getQuery :<|> getContent)

stmMapToList :: STM.Map k v -> STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STM.stream

provisionLayer :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
               => Text -> LayerQuery -> m NoContent
provisionLayer l query = do
  r <- ask
  let (ls, cfgFile, originalCfg) = (,,) <$> _ssStateLayers <*> _ssConfigFile <*> _ssOriginalConfig $ r
  lastModifiedTime <- liftIO getCurrentTime
  newLayers <- liftIO . atomically $ do
    STM.insert (Layer (unLayerQuery query) lastModifiedTime) l ls
    stmMapToList ls
  liftIO $ LBS.writeFile cfgFile (encodePretty (originalCfg {_configLayers = fromList newLayers}))
  pure NoContent

returnConfiguration ::(MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
               => m Types.InputConfig
returnConfiguration = do
  cfgFile <- asks _ssConfigFile
  configBs <- liftIO $ LBS.readFile cfgFile
  case eitherDecode configBs of
    Left e -> throwError $ err500 { errBody = LBS.pack $ show e }
    Right c -> pure c

getQuery :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
         => Text -> Integer -> Integer -> Integer -> m Text
getQuery l z x y = do
  layer <- getLayerOrThrow l
  query <- mkQuery layer (Coordinates (ZoomLevel z) (GoogleTileCoords x y))
  pure query

getContent :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Integer -> Integer -> Text -> m (Headers '[Header "Last-Modified" String] BS.ByteString)
getContent l z x stringY
  | ".mvt" `T.isSuffixOf` stringY = getAnything getTile l z x stringY
  | ".json" `T.isSuffixOf` stringY = getAnything getJson l z x stringY
  | otherwise = throwError $ err400 { errBody = "Unknown request: " <> fromStrict (TE.encodeUtf8 stringY) }

getAnything :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
            => (t -> Coordinates -> m a) -> t -> Integer -> Integer -> Text -> m a
getAnything f l z x stringY =
  case getIntY stringY of
    Left e -> fail $ show e
    Right (y, _) -> f l (Coordinates (ZoomLevel z) (GoogleTileCoords x y))
  where
    getIntY s = decimal $ T.takeWhile isNumber s

getTile :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
         => Text -> Coordinates -> m (Headers '[Header "Last-Modified" String] BS.ByteString)
getTile l zxy = do
  layer <- getLayerOrThrow l
  geoJson <- getJson' layer zxy
  buffer <- asks (^. ssBuffer)
  let gtc = DGT.GoogleTileCoords (_z . _zl $ zxy) (DGT.Coords (_x . _xy $ zxy) (_y . _xy $ zxy))
      config = DGT.Config gtc (DGT.Pixels $ _pixels defaultTileSize + _pixels buffer) l 2
      eet = DGM.createMvt config geoJson
      tile = VT.encode . VT.untile $ VVT.VectorTile (fromList [(l, eet)])
  checkEmpty tile layer
  -- case eet of
  --   Left e -> throwError $ err500 { errBody = fromStrict $ TE.encodeUtf8 e }
  --   Right tile -> checkEmpty tile layer

checkEmpty :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
           => BS.ByteString -> Layer -> m (Headers '[Header "Last-Modified" String] BS.ByteString)
checkEmpty tile layer
  | BS.null tile = throwError $ err204 { errHeaders = [(hLastModified, BS8.pack $ lastModified layer)] }
  | otherwise = pure $ addHeader (lastModified layer) tile

getJson :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
        => Text -> Coordinates -> m (Headers '[Header "Last-Modified" String] BS.ByteString)
getJson l zxy = do
  layer <- getLayerOrThrow l
  geoJson <- getJson' layer zxy
  pure $ addHeader (lastModified layer) (toStrict $ encode geoJson)

getJson' :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
         => Layer -> Coordinates -> m GJ.FeatureCollection
getJson' layer zxy = do
  errorOrTfs <- findFeatures layer zxy
  case errorOrTfs of
    Left e -> throwError $ err500 { errBody = LBS.pack $ show e }
    Right tfs -> pure $ GJ.FeatureCollection Nothing (mkGeoJSON tfs)

getLayerOrThrow :: (MonadIO m, MonadReader ServerState m, MonadError ServantErr m)
                => Text -> m Layer
getLayerOrThrow l = do
  errorOrLayer <- getLayer l
  case errorOrLayer of
    Left LayerNotFound -> throwError $ err404 { errBody = "Layer not found :-(" }
    Right layer -> pure layer
