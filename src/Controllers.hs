{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Controllers where

import           Control.Lens               ((^.))
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class as RC
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString            as BS
import           Data.ByteString.Char8      as BS8
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Char
import qualified Data.Geography.GeoJSON     as GJ
import           Data.Map                   as M
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Encoding         as TE
import           Data.Text.Read             as DTR
import           Data.Time
import           GHC.Conc
import           ListT
import           Network.HTTP.Types.Header  (hLastModified)
import           Servant
import           STMContainers.Map          as STM

import           DB
import           Routes
import           Tile
import           Types

hastileServer :: ServerT HastileApi ActionHandler
hastileServer = returnConfiguration :<|> provisionLayer :<|> getQuery :<|> getContent

stmMapToList :: STM.Map k v -> STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STM.stream

provisionLayer :: T.Text -> LayerQuery -> ActionHandler NoContent
provisionLayer l query = do
  r <- RC.ask
  let (ls, cfgFile, originalCfg) = (,,) <$> _ssStateLayers <*> _ssConfigFile <*> _ssOriginalConfig $ r
  lastModifiedTime <- liftIO getCurrentTime
  newLayers <- liftIO . atomically $ do
    STM.insert (Layer (unLayerQuery query) lastModifiedTime) l ls
    stmMapToList ls
  liftIO $ LBS.writeFile cfgFile (encodePretty (originalCfg {_configLayers = fromList newLayers}))
  pure NoContent

returnConfiguration :: ActionHandler Types.InputConfig
returnConfiguration = do
  cfgFile <- RC.asks _ssConfigFile
  configBs <- liftIO $ LBS.readFile cfgFile
  case eitherDecode configBs of
    Left e  -> throwError $ err500 { errBody = LBS.pack $ show e }
    Right c -> pure c

getQuery :: T.Text -> Integer -> Integer -> Integer -> ActionHandler T.Text
getQuery l z x y = do
  layer <- getLayerOrThrow l
  query <- mkQuery layer (Coordinates (ZoomLevel z) (GoogleTileCoords x y))
  pure query

getContent :: T.Text -> Integer -> Integer -> T.Text -> ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
getContent l z x stringY
  | ".mvt" `T.isSuffixOf` stringY = getAnything getTile l z x stringY
  | ".json" `T.isSuffixOf` stringY = getAnything getJson l z x stringY
  | otherwise = throwError $ err400 { errBody = "Unknown request: " <> fromStrict (TE.encodeUtf8 stringY) }

getAnything :: (t -> Coordinates -> ActionHandler a) -> t -> Integer -> Integer -> T.Text -> ActionHandler a
getAnything f l z x stringY =
  case getIntY stringY of
    Left e       -> fail $ show e
    Right (y, _) -> f l (Coordinates (ZoomLevel z) (GoogleTileCoords x y))
  where
    getIntY s = decimal $ T.takeWhile isNumber s

getTile :: T.Text -> Coordinates -> ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
getTile l zxy = do
  layer <- getLayerOrThrow l
  geoJson <- getJson' layer zxy
  buffer <- RC.asks (^. ssBuffer)
  tile <- liftIO $ mkTile l zxy buffer geoJson
  checkEmpty tile layer

checkEmpty :: BS.ByteString -> Layer -> ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
checkEmpty tile layer
  | BS.null tile = throwError $ err204 { errHeaders = [(hLastModified, BS8.pack $ lastModified layer)] }
  | otherwise = pure $ addHeader (lastModified layer) tile

getJson :: T.Text -> Coordinates -> ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
getJson l zxy = do
  layer <- getLayerOrThrow l
  geoJson <- getJson' layer zxy
  pure $ addHeader (lastModified layer) (toStrict $ encode geoJson)

getJson' :: Layer -> Coordinates -> ActionHandler GJ.FeatureCollection
getJson' layer zxy = do
  errorOrTfs <- findFeatures layer zxy
  case errorOrTfs of
    Left e    -> throwError $ err500 { errBody = LBS.pack $ show e }
    Right tfs -> pure $ GJ.FeatureCollection Nothing (mkGeoJSON tfs)

getLayerOrThrow :: T.Text -> ActionHandler Layer
getLayerOrThrow l = do
  errorOrLayer <- getLayer l
  case errorOrLayer of
    Left LayerNotFound -> throwError $ err404 { errBody = "Layer not found :-(" }
    Right layer -> pure layer