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

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.Aeson
import           Data.ByteString            as BS
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Char
import           Data.Map                   as M
import           Data.Monoid
import           Data.Text                  as T
import           Data.Text.Encoding         as TE
import           Data.Text.Read             as DTR
import           Data.Time
import           GHC.Conc
import           ListT
import           Servant
import           STMContainers.Map          as STM

-- import           MapboxVectorTile
import           DB
import           Routes
import           Tile
import           Types

hastileService :: ServerState -> Server HastileApi
hastileService state =
  enter (runReaderTNat state) (provisionLayer :<|> getQuery :<|> getContent)

stmMapToList :: STM.Map k v -> STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STM.stream

provisionLayer :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
         => Text -> Text -> m NoContent
provisionLayer l query = do
  ls <- asks _ssStateLayers
  cfgFile <- asks _ssConfigFile
  lastModifiedTime <- liftIO getCurrentTime
  _ <- liftIO $ atomically $ STM.insert (Layer query lastModifiedTime) l ls
  newLayers <- liftIO $ atomically $ stmMapToList ls
  let newConfig = Config "asdf" Nothing Nothing Nothing Nothing (fromList newLayers)
  _ <- liftIO $ LBS.writeFile cfgFile (encode newConfig)
  pure NoContent

getQuery :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
         => Text -> Integer -> Integer -> Integer -> m Text
getQuery l z x y = getQuery' l (Coordinates (ZoomLevel z) (GoogleTileCoords x y))

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
  pp <- asks _ssPluginDir
  geoJson <- getJson' l zxy
  lastModified <- getLastModified l
  eet <- liftIO $ tileReturn geoJson pp
  case eet of
    Left e -> throwError $ err500 { errBody = fromStrict $ TE.encodeUtf8 e }
    Right tile -> pure $ addHeader lastModified tile
  where
--    tileReturn geoJson' pp' = fromGeoJSON defaultTileSize geoJson' l pp' zxy
    tileReturn _ _ = undefined

getJson :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
         => Text -> Coordinates -> m (Headers '[Header "Last-Modified" String] BS.ByteString)
getJson l zxy = do
  lastModified <- getLastModified l
  geoJson <- getJson' l zxy
  pure $ addHeader lastModified . toStrict . encode $ geoJson

getJson' :: (MonadIO m, MonadError ServantErr m, MonadReader ServerState m)
          => Text -> Coordinates -> m GeoJson
getJson' l zxy = do
  errorOrTfs <- findFeatures l zxy
  case errorOrTfs of
    Left e -> throwError $ err500 { errBody = LBS.pack $ show e }
    Right tfs -> pure $ mkGeoJSON tfs

mkGeoJSON :: [TileFeature] -> GeoJson
mkGeoJSON tfs = M.fromList [ ("type", String "FeatureCollection")
                             , ("features", toJSON . fmap mkFeature $ tfs)
                             ]

mkFeature :: TileFeature -> Value
mkFeature tf = toJSON featureMap
  where featureMap = M.fromList [ ("type", String "Feature")
                                , ("geometry", _tfGeometry tf)
                                , ("properties", toJSON . _tfProperties $ tf)
                                ] :: M.Map Text Value
