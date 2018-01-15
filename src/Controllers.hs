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
import qualified Data.Aeson                 as A
import           Data.Aeson.Encode.Pretty
import           Data.ByteString            as BS
import           Data.ByteString.Char8      as BS8
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Char
import qualified Data.Geometry.Types.Types  as DGT
import qualified Data.Geospatial            as DG
import           Data.Map                   as M
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Encoding         as TE
import           Data.Text.Read             as DTR
import           Data.Time
import           GHC.Conc
import           ListT
import           Network.HTTP.Types.Header  (hLastModified)
import           Numeric.Natural            (Natural)
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

provisionLayer :: T.Text -> LayerRequest -> ActionHandler NoContent
provisionLayer l request = do
  r <- RC.ask
  let (ls, cfgFile, originalCfg) = (,,) <$> _ssStateLayers <*> _ssConfigFile <*> _ssOriginalConfig $ r
  lastModifiedTime <- liftIO getCurrentTime
  newLayers <- liftIO . atomically $ do
    STM.insert (requestToLayer request lastModifiedTime) l ls
    stmMapToList ls
  liftIO $ LBS.writeFile cfgFile (encodePretty (originalCfg {_configLayers = fromList newLayers}))
  pure NoContent

returnConfiguration :: ActionHandler Types.InputConfig
returnConfiguration = do
  cfgFile <- RC.asks _ssConfigFile
  configBs <- liftIO $ LBS.readFile cfgFile
  case A.eitherDecode configBs of
    Left e  -> throwError $ err500 { errBody = LBS.pack $ show e }
    Right c -> pure c

getQuery :: T.Text -> Natural -> Integer -> Integer -> ActionHandler T.Text
getQuery l z x y = do
  layer <- getLayerOrThrow l
  query <- mkQuery layer (DGT.mkGoogleTileCoords z x y)
  pure query

getContent :: T.Text -> Natural -> Integer -> T.Text -> ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
getContent l z x stringY
  | ".mvt" `T.isSuffixOf` stringY = getAnything getTile l z x stringY
  | ".json" `T.isSuffixOf` stringY = getAnything getJson l z x stringY
  | otherwise = throwError $ err400 { errBody = "Unknown request: " <> fromStrict (TE.encodeUtf8 stringY) }

getAnything :: (t -> DGT.GoogleTileCoords -> ActionHandler a) -> t -> Natural -> Integer -> T.Text -> ActionHandler a
getAnything f l z x stringY =
  case getIntY stringY of
    Left e       -> fail $ show e
    Right (y, _) -> f l (DGT.mkGoogleTileCoords z x y)
  where
    getIntY s = decimal $ T.takeWhile isNumber s

getTile :: T.Text -> DGT.GoogleTileCoords -> ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
getTile l zxy = do
  layer <- getLayerOrThrow l
  geoJson <- getJson' layer zxy
  buffer <- RC.asks (^. ssBuffer)
  tile <- liftIO $ mkTile l zxy buffer 1 geoJson
  checkEmpty tile layer

checkEmpty :: BS.ByteString -> Layer -> ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
checkEmpty tile layer
  | BS.null tile = throwError $ err204 { errHeaders = [(hLastModified, BS8.pack $ lastModified layer)] }
  | otherwise = pure $ addHeader (lastModified layer) tile

getJson :: T.Text -> DGT.GoogleTileCoords -> ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
getJson l zxy = do
  layer <- getLayerOrThrow l
  geoJson <- getJson' layer zxy
  pure $ addHeader (lastModified layer) (toStrict $ A.encode geoJson)

getJson' :: Layer -> DGT.GoogleTileCoords -> ActionHandler (DG.GeoFeatureCollection A.Value)
getJson' layer zxy = do
  errorOrTfs <- findFeatures layer zxy
  case errorOrTfs of
    Left e    -> throwError $ err500 { errBody = LBS.pack $ show e }
    Right tfs -> pure $ DG.GeoFeatureCollection Nothing (mkGeoJSON tfs)

getLayerOrThrow :: T.Text -> ActionHandler Layer
getLayerOrThrow l = do
  errorOrLayer <- getLayer l
  case errorOrLayer of
    Left LayerNotFound -> throwError $ err404 { errBody = "Layer not found :-(" }
    Right layer -> pure layer
