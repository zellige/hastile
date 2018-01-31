{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import           Control.Lens                 (Lens', makeLenses)
import           Control.Monad.Except         (MonadError)
import           Control.Monad.Reader         (MonadIO, MonadReader, ReaderT)
import           Data.Aeson                   as A
import           Data.Aeson.Types             as AT
import qualified Data.ByteString              as BS
import           Data.ByteString.Lazy         (ByteString, fromStrict)
import qualified Data.Geospatial              as DG
import           Data.Map.Strict              as M
import           Data.Maybe                   (catMaybes)
import           Data.Text                    as T
import           Data.Time
import           Data.Typeable
import           Hasql.Pool                   as P
import qualified Network.HTTP.Media           as HM
import           Options.Generic
import           Servant
import           STMContainers.Map            as STM

import qualified Data.Geometry.Types.Simplify as DGTS
import qualified Data.Geometry.Types.Types    as DGTT

defaultTileSize :: DGTT.Pixels
defaultTileSize = 2048

-- Layer types

data LayerRequest = LayerRequest
  { _lrQuery      :: Text
  , _lrQuantize   :: DGTT.Pixels
  , _lrAlgorithms :: Algorithms
  } deriving (Show, Eq)

instance FromJSON LayerRequest where
  parseJSON = withObject "LayerRequest" $ \o -> LayerRequest
    <$> o .: "query"
    <*> o .: "quantize"
    <*> o .: "simplify"

instance ToJSON LayerRequest where
  toJSON lr = object
    [ "query"    .= _lrQuery lr
    , "quantize" .= _lrQuantize lr
    , "simplify" .= _lrAlgorithms lr
    ]

requestToLayer :: LayerRequest -> UTCTime -> Layer
requestToLayer (LayerRequest que qua al) time = Layer que time qua al

data Layer = Layer
  { _layerQuery        :: Text
  , _layerLastModified :: UTCTime
  , _layerQuantize     :: DGTT.Pixels
  , _layerAlgorithms   :: Algorithms
  } deriving (Show, Eq, Generic)

instance FromJSON Layer where
  parseJSON = withObject "Layer" $ \o -> Layer
    <$> o .: "query"
    <*> o .: "last-modified"
    <*> o .: "quantize"
    <*> o .: "simplify"

instance ToJSON Layer where
  toJSON l = object
    [ "query"         .= _layerQuery l
    , "last-modified" .= _layerLastModified l
    , "quantize"      .= _layerQuantize l
    , "simplify"      .= _layerAlgorithms l
    ]

-- Zoom dependant simplification algorithms

-- TODO use map Strict
type Algorithms = M.Map DGTT.ZoomLevel DGTS.SimplificationAlgorithm

getAlgorithm :: DGTT.ZoomLevel -> Layer -> DGTS.SimplificationAlgorithm
getAlgorithm z layer = getAlgorithm' z (_layerAlgorithms layer)

getAlgorithm' :: DGTT.ZoomLevel -> Algorithms -> DGTS.SimplificationAlgorithm
getAlgorithm' z algos = case M.lookupGE z algos of
  Nothing        -> DGTS.NoAlgorithm
  Just (_, algo) -> algo

-- Config

data InputConfig = InputConfig
  { _inputConfigPgConnection       :: Text
  , _inputConfigPgPoolSize         :: Maybe Int
  , _inputConfigPgTimeout          :: Maybe NominalDiffTime
  , _inputConfigMapnikInputPlugins :: Maybe FilePath
  , _inputConfigPort               :: Maybe Int
  , _inputConfigLayers             :: M.Map Text Layer
  , _inputConfigTileBuffer         :: Maybe DGTT.Pixels
  } deriving (Show, Generic)

makeLenses ''InputConfig

instance ToJSON InputConfig where
  toJSON ic = object $ catMaybes
    [ ("db-connection" .=)        <$> Just (_inputConfigPgConnection ic)
    , ("db-pool-size" .=)         <$> _inputConfigPgPoolSize ic
    , ("db-timeout" .=)           <$> _inputConfigPgTimeout ic
    , ("mapnik-input-plugins" .=) <$> _inputConfigMapnikInputPlugins ic
    , ("port" .=)                 <$> _inputConfigPort ic
    , ("layers" .=)               <$> Just (_inputConfigLayers ic)
    , ("tile-buffer" .=)          <$> Just (_inputConfigTileBuffer ic)
    ]

instance FromJSON InputConfig where
  parseJSON = withObject "Config" $ \o -> InputConfig
    <$> o .:  "db-connection"
    <*> o .:? "db-pool-size"
    <*> o .:? "db-timeout"
    <*> o .:? "mapnik-input-plugins"
    <*> o .:? "port"
    <*> o .:  "layers"
    <*> o .:? "tile-buffer"

emptyInputConfig :: InputConfig
emptyInputConfig = InputConfig "" Nothing Nothing Nothing Nothing (fromList []) Nothing

data Config = Config
  { _configPgConnection       :: Text
  , _configPgPoolSize         :: Int
  , _configPgTimeout          :: NominalDiffTime
  , _configMapnikInputPlugins :: FilePath
  , _configPort               :: Int
  , _configLayers             :: M.Map Text Layer
  , _configTileBuffer         :: DGTT.Pixels
  } deriving (Show, Generic)

makeLenses ''Config

instance ToJSON Config where
  toJSON c = object
    [ "db-connection"        .= _configPgConnection c
    , "db-pool-size"         .= _configPgPoolSize c
    , "db-timeout"           .= _configPgTimeout c
    , "mapnik-input-plugins" .= _configMapnikInputPlugins c
    , "port"                 .= _configPort c
    , "layers"               .= _configLayers c
    , "tile-buffer"          .= _configTileBuffer c
    ]

-- Types

data AlreadyJSON deriving Typeable

instance Accept AlreadyJSON where
    contentType _ = "application" HM.// "json"

instance MimeRender AlreadyJSON Data.ByteString.Lazy.ByteString where
    mimeRender _ = id

instance MimeRender AlreadyJSON BS.ByteString where
    mimeRender _ = fromStrict

data MapboxVectorTile deriving Typeable

instance Accept MapboxVectorTile where
    contentType _ = "application" HM.// "vnd.mapbox-vector-tile"

instance MimeRender MapboxVectorTile Data.ByteString.Lazy.ByteString where
    mimeRender _ = id

instance MimeRender MapboxVectorTile BS.ByteString where
    mimeRender _ = fromStrict

newtype TileFeature = TileFeature
  { unTileFeature :: Value
  } deriving (Show, Eq)

-- Command line args

newtype CmdLine = CmdLine
  { configFile :: FilePath
  } deriving Generic

instance ParseRecord CmdLine

-- App types

data ServerState = ServerState
  { _ssPool           :: P.Pool
  , _ssPluginDir      :: FilePath
  , _ssConfigFile     :: FilePath
  , _ssOriginalConfig :: Config
  , _ssStateLayers    :: STM.Map Text Layer
  }

makeLenses ''ServerState

ssBuffer :: Lens' ServerState DGTT.Pixels
ssBuffer = ssOriginalConfig . configTileBuffer

newtype ActionHandler a = ActionHandler
  { runActionHandler :: ReaderT ServerState Handler a
  } deriving (Functor, Applicative, Monad, MonadReader ServerState, MonadError ServantErr, MonadIO)

-- Helpers

mkGeoJSON :: [Value] -> [DG.GeoFeature AT.Value]
mkGeoJSON = fmap (x . parseEither parseJSON)
  where
    x = either (\_ -> DG.GeoFeature Nothing (DG.Collection []) Null Nothing) id

err204 :: ServantErr
err204 = ServantErr { errHTTPCode = 204
                    , errReasonPhrase = "No Content"
                    , errBody = ""
                    , errHeaders = []
                    }
