{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Controllers where

import           Control.Lens               ((^.))
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader.Class as RC
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encode.Pretty   as AE
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Char                  as C
import qualified Data.Geospatial            as DG
import           Data.Map                   as M
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Read             as DTR
import           Data.Time
import           GHC.Conc
import           ListT
import           Network.HTTP.Types.Header  (hLastModified)
import           Numeric.Natural            (Natural)
import qualified Servant                    as S
import qualified STMContainers.Map          as STM

import qualified Data.Geometry.Types.Types  as DGTT

import qualified Hastile.DB                 as DB
import qualified Hastile.Routes             as Routes
import qualified Hastile.Tile               as T
import qualified Hastile.Types              as T

hastileServer :: S.ServerT Routes.HastileApi T.ActionHandler
hastileServer = returnConfiguration S.:<|> createNewLayer S.:<|> layerServer

layerServer :: S.ServerT Routes.LayerApi T.ActionHandler
layerServer l = provisionLayer l S.:<|> coordsServer l

coordsServer :: T.Text -> Natural -> Natural -> S.ServerT Routes.HastileContentApi T.ActionHandler
coordsServer l z x = getQuery l z x S.:<|> getContent l z x

stmMapToList :: STM.Map k v -> STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STM.stream

createNewLayer :: T.LayerRequestList -> T.ActionHandler S.NoContent
createNewLayer (T.LayerRequestList layerRequests) =
  newLayer (\lastModifiedTime ls -> mapM_ (\lr -> STM.insert (T.requestToLayer (T._newLayerRequestName lr) (T._newLayerRequestSettings lr) lastModifiedTime) (T._newLayerRequestName lr) ls) layerRequests)

provisionLayer :: T.Text -> T.LayerSettings -> T.ActionHandler S.NoContent
provisionLayer l settings =
  newLayer (\lastModifiedTime -> STM.insert (T.requestToLayer l settings lastModifiedTime) l)

newLayer :: (MonadIO m, RC.MonadReader T.ServerState m) => (UTCTime -> STM.Map T.Text T.Layer -> STM a) -> m S.NoContent
newLayer b = do
  r <- RC.ask
  lastModifiedTime <- liftIO getCurrentTime
  let (ls, cfgFile, originalCfg) = (,,) <$> T._ssStateLayers <*> T._ssConfigFile <*> T._ssOriginalConfig $ r
  newLayers <- liftIO . atomically $ do
    _ <- b lastModifiedTime ls
    stmMapToList ls
  let newNewLayers = fmap (\(k, v) -> (k, T.layerToLayerDetails v)) newLayers
  liftIO $ LBS8.writeFile cfgFile (AE.encodePretty (originalCfg {T._configLayers = fromList newNewLayers}))
  pure S.NoContent

returnConfiguration :: T.ActionHandler T.InputConfig
returnConfiguration = do
  cfgFile <- RC.asks T._ssConfigFile
  configBs <- liftIO $ LBS8.readFile cfgFile
  case A.eitherDecode configBs of
    Left e  -> throwError $ S.err500 { S.errBody = LBS8.pack $ show e }
    Right c -> pure c

getQuery :: T.Text -> Natural -> Natural -> Natural -> T.ActionHandler T.Text
getQuery l z x y = do
  layer <- getLayerOrThrow l
  DB.mkQuery layer z (x, y)

getContent :: T.Text -> Natural -> Natural -> T.Text -> Maybe T.Text -> T.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
getContent l z x stringY maybeIfModified =
  do
    layer <- getLayerOrThrow l
    if DB.isModified layer maybeIfModified
      then getContent' layer z x stringY
      else throwError S.err304

getContent' :: T.Layer -> Natural -> Natural -> T.Text -> T.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
getContent' l z x stringY
  | ".mvt" `T.isSuffixOf` stringY = getAnything getTile l z x stringY
  | ".json" `T.isSuffixOf` stringY = getAnything getJson l z x stringY
  | otherwise = throwError $ S.err400 { S.errBody = "Unknown request: " <> LBS8.fromStrict (TE.encodeUtf8 stringY) }

getAnything :: (t -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> T.ActionHandler a) -> t -> DGTT.ZoomLevel -> DGTT.Pixels -> T.Text -> T.ActionHandler a
getAnything f layer z x stringY =
  case getY stringY of
    Left e       -> fail $ show e
    Right (y, _) -> f layer z (x, y)
  where
    getY s = DTR.decimal $ T.takeWhile C.isNumber s

getTile :: T.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> T.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
getTile layer z xy = do
  geoJson <- getJson' layer z xy
  buffer  <- RC.asks (^. T.ssBuffer)
  let simplificationAlgorithm = T.getAlgorithm z layer
  tile    <- liftIO $ T.mkTile (T._layerName layer) z xy buffer (T._layerQuantize layer) simplificationAlgorithm geoJson
  checkEmpty tile layer

checkEmpty :: BS.ByteString -> T.Layer -> T.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
checkEmpty tile layer
  | BS.null tile = throwError $ T.err204 { S.errHeaders = [(hLastModified, TE.encodeUtf8 $ DB.lastModified layer)] }
  | otherwise = pure $ S.addHeader (DB.lastModified layer) tile

getJson :: T.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> T.ActionHandler (S.Headers '[S.Header "Last-Modified" T.Text] BS.ByteString)
getJson layer z xy = S.addHeader (DB.lastModified layer) . LBS8.toStrict . A.encode <$> getJson' layer z xy

getJson' :: T.Layer -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> T.ActionHandler (DG.GeoFeatureCollection A.Value)
getJson' layer z xy = do
  errorOrTfs <- DB.findFeatures layer z xy
  case errorOrTfs of
    Left e    -> throwError $ S.err500 { S.errBody = LBS8.pack $ show e }
    Right tfs -> pure $ DG.GeoFeatureCollection Nothing (T.mkGeoJSON tfs)

getLayerOrThrow :: T.Text -> T.ActionHandler T.Layer
getLayerOrThrow l = do
  errorOrLayer <- DB.getLayer l
  case errorOrLayer of
    Left DB.LayerNotFound -> throwError $ S.err404 { S.errBody = "Layer not found :-(" }
    Right layer -> pure layer
