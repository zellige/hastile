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

import qualified Data.Geometry.Types.Types  as DGTT

import qualified DB
import qualified Routes
import qualified Tile                       as T
import qualified Types                      as T

hastileServer :: ServerT Routes.HastileApi T.ActionHandler
hastileServer = returnConfiguration :<|> createNewLayer :<|> layerServer

layerServer :: ServerT Routes.LayerApi T.ActionHandler
layerServer l = provisionLayer l :<|> coordsServer l

coordsServer :: T.Text -> Natural -> Natural -> ServerT Routes.HastileContentApi T.ActionHandler
coordsServer l z x = getQuery l z x :<|> getContent l z x

stmMapToList :: STM.Map k v -> STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STM.stream

createNewLayer :: T.LayerRequestList -> T.ActionHandler NoContent
createNewLayer (T.LayerRequestList layerRequests) =
  newLayer (\lastModifiedTime ls -> mapM_ (\lr -> STM.insert (T.requestToLayer (T._newLayerRequestSettings lr) lastModifiedTime) (T._newLayerRequestName lr) ls) layerRequests)

provisionLayer :: T.Text -> T.LayerSettings -> T.ActionHandler NoContent
provisionLayer l settings =
  newLayer (\lastModifiedTime -> STM.insert (T.requestToLayer settings lastModifiedTime) l)

newLayer :: (MonadIO m, MonadReader T.ServerState m) => (UTCTime -> STM.Map T.Text T.Layer -> STM a) -> m NoContent
newLayer b = do
  r <- RC.ask
  lastModifiedTime <- liftIO getCurrentTime
  let (ls, cfgFile, originalCfg) = (,,) <$> T._ssStateLayers <*> T._ssConfigFile <*> T._ssOriginalConfig $ r
  newLayers <- liftIO . atomically $ do
    _ <- b lastModifiedTime ls
    stmMapToList ls
  liftIO $ LBS.writeFile cfgFile (encodePretty (originalCfg {T._configLayers = fromList newLayers}))
  pure NoContent

returnConfiguration :: T.ActionHandler T.InputConfig
returnConfiguration = do
  cfgFile <- RC.asks T._ssConfigFile
  configBs <- liftIO $ LBS.readFile cfgFile
  case A.eitherDecode configBs of
    Left e  -> throwError $ err500 { errBody = LBS.pack $ show e }
    Right c -> pure c

getQuery :: T.Text -> Natural -> Natural -> Natural -> T.ActionHandler T.Text
getQuery l z x y = do
  layer <- getLayerOrThrow l
  DB.mkQuery layer z (x, y)

getContent :: T.Text -> Natural -> Natural -> T.Text -> Maybe T.Text -> T.ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
getContent l z x stringY _
  | ".mvt" `T.isSuffixOf` stringY = getAnything getTile l z x stringY
  | ".json" `T.isSuffixOf` stringY = getAnything getJson l z x stringY
  | otherwise = throwError $ err400 { errBody = "Unknown request: " <> fromStrict (TE.encodeUtf8 stringY) }

getAnything :: (t -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> T.ActionHandler a) -> t -> DGTT.ZoomLevel -> DGTT.Pixels -> T.Text -> T.ActionHandler a
getAnything f l z x stringY =
  case getY stringY of
    Left e       -> fail $ show e
    Right (y, _) -> f l z (x, y)
  where
    getY s = decimal $ T.takeWhile isNumber s

getTile :: T.Text -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> T.ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
getTile l z xy = do
  layer   <- getLayerOrThrow l
  geoJson <- getJson' layer z xy
  buffer  <- RC.asks (^. T.ssBuffer)
  let simplificationAlgorithm = T.getAlgorithm z layer
  tile    <- liftIO $ T.mkTile l z xy buffer (T._layerQuantize layer) simplificationAlgorithm geoJson
  checkEmpty tile layer

checkEmpty :: BS.ByteString -> T.Layer -> T.ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
checkEmpty tile layer
  | BS.null tile = throwError $ T.err204 { errHeaders = [(hLastModified, BS8.pack $ DB.lastModified layer)] }
  | otherwise = pure $ addHeader (DB.lastModified layer) tile

getJson :: T.Text -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> T.ActionHandler (Headers '[Header "Last-Modified" String] BS.ByteString)
getJson l z xy = do
  layer <- getLayerOrThrow l
  geoJson <- getJson' layer z xy
  pure $ addHeader (DB.lastModified layer) (toStrict $ A.encode geoJson)

getJson' :: T.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> T.ActionHandler (DG.GeoFeatureCollection A.Value)
getJson' layer z xy = do
  errorOrTfs <- DB.findFeatures layer z xy
  case errorOrTfs of
    Left e    -> throwError $ err500 { errBody = LBS.pack $ show e }
    Right tfs -> pure $ DG.GeoFeatureCollection Nothing (T.mkGeoJSON tfs)

getLayerOrThrow :: T.Text -> T.ActionHandler T.Layer
getLayerOrThrow l = do
  errorOrLayer <- DB.getLayer l
  case errorOrLayer of
    Left DB.LayerNotFound -> throwError $ err404 { errBody = "Layer not found :-(" }
    Right layer -> pure layer
