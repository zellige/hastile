{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Types where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString      as BS
import           Data.ByteString.Lazy (ByteString, fromStrict)
import           Data.Map             as M
import           Data.Maybe
import           Data.Text            as T
import           Data.Time
import           Data.Typeable
import           Hasql.Pool           as P
import qualified Network.HTTP.Media   as M
import           Options.Generic
import           Servant
import           STMContainers.Map    as STM

type GeoJson = M.Map Text Value

newtype ZoomLevel = ZoomLevel { _z :: Integer
                              } deriving (Show, Eq, Num)

data GoogleTileCoords = GoogleTileCoords { _x :: Integer
                                         , _y :: Integer
                                         } deriving (Eq, Show)

data Coordinates = Coordinates { _zl :: ZoomLevel
                               , _xy :: GoogleTileCoords
                               } deriving (Show, Eq)

data CmdLine = CmdLine { configFile :: FilePath
                       } deriving Generic
instance ParseRecord CmdLine

newtype LayerQuery = LayerQuery { unLayerQuery :: Text } deriving (Show, Eq)

instance ToJSON LayerQuery where
  toJSON (LayerQuery lq) = object [ "query" .= lq ]

instance FromJSON LayerQuery where
  parseJSON (Object o) = LayerQuery <$> o .: "query"
  parseJSON _ = Control.Applicative.empty

data Layer = Layer { _layerQuery        :: Text
                   , _layerLastModified :: UTCTime
                   } deriving (Show, Eq, Generic)

instance FromJSON Layer where
  parseJSON (Object o) =
       Layer <$> o .: "query" <*> o .: "last-modified"
  parseJSON _ = Control.Applicative.empty

data Config = Config { _configPgConnection       :: Text
                     , _configPgPoolSize         :: Maybe Int
                     , _configPgTimeout          :: Maybe NominalDiffTime
                     , _configMapnikInputPlugins :: Maybe FilePath
                     , _configPort               :: Maybe Int
                     , _configLayers             :: M.Map Text Layer
                     } deriving (Show, Generic)

emptyConfig :: Config
emptyConfig = Config "" Nothing Nothing Nothing Nothing (fromList [])

instance FromJSON Config where
  parseJSON (Object o) =
       Config <$> o .: "db-connection" <*> o .:? "db-pool-size" <*> o .:? "db-timeout" <*>
          o .:? "mapnik-input-plugins" <*> o .:? "port" <*> o .: "layers"
  parseJSON _ = Control.Applicative.empty

instance ToJSON Config where
  toJSON c = object $ catMaybes
    [
      ("db-connection" .=) <$> Just (_configPgConnection c),
      ("db-pool-size" .=) <$> _configPgPoolSize c,
      ("db-timeout" .=) <$> _configPgTimeout c,
      ("mapnik-input-plugins" .=) <$> _configMapnikInputPlugins c,
      ("port" .=) <$> _configPort c,
      ("layers" .=) <$> Just (_configLayers c)
    ]

instance ToJSON Layer where
  toJSON l = object
    [  "query" .= _layerQuery l,
       "last-modified" .= _layerLastModified l
    ]

-- TODO: make lenses!
data ServerState = ServerState { _ssPool           :: P.Pool
                               , _ssPluginDir      :: FilePath
                               , _ssConfigFile     :: FilePath
                               , _ssOriginalConfig :: Config
                               , _ssStateLayers    :: STM.Map Text Layer
                               }

newtype TileFeature = TileFeature { unTileFeature :: Value } deriving (Show, Eq)

mkGeoJSON :: [TileFeature] -> GeoJson
mkGeoJSON tfs = M.fromList [ ("type", String "FeatureCollection")
                           , ("features", toJSON . fmap unTileFeature $ tfs)
                           ]

err204 :: ServantErr
err204 = ServantErr { errHTTPCode = 204
                    , errReasonPhrase = "No Content"
                    , errBody = ""
                    , errHeaders = []
                    }

data AlreadyJSON deriving Typeable

instance Accept AlreadyJSON where
    contentType _ = "application" M.// "json"

instance MimeRender AlreadyJSON Data.ByteString.Lazy.ByteString where
    mimeRender _ = id

instance MimeRender AlreadyJSON BS.ByteString where
    mimeRender _ = fromStrict

data MapboxVectorTile deriving Typeable

instance Accept MapboxVectorTile where
    contentType _ = "application" M.// "vnd.mapbox-vector-tile"

instance MimeRender MapboxVectorTile Data.ByteString.Lazy.ByteString where
    mimeRender _ = id

instance MimeRender MapboxVectorTile BS.ByteString where
    mimeRender _ = fromStrict
