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

import           Control.Applicative
import           Control.Lens           (Lens', makeLenses)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy   (ByteString, fromStrict)
import qualified Data.Geography.GeoJSON as GJ
import           Data.Map               as M
import           Data.Maybe             (catMaybes)
import           Data.Monoid            ((<>))
import           Data.Text              as T
import           Data.Time
import           Data.Typeable
import           Hasql.Pool             as P
import qualified Network.HTTP.Media     as HM
import           Options.Generic
import           Servant
import           STMContainers.Map      as STM


defaultTileSize :: Pixels
defaultTileSize = Pixels 2048

newtype ZoomLevel = ZoomLevel { _z :: Integer
                              } deriving (Show, Eq, Num)

data GoogleTileCoords = GoogleTileCoords { _x :: Integer
                                         , _y :: Integer
                                         } deriving (Eq, Show)

data Coordinates = Coordinates { _zl :: ZoomLevel
                               , _xy :: GoogleTileCoords
                               } deriving (Show, Eq)

newtype Pixels = Pixels { _pixels :: Int } deriving (Show, Eq, Num)

instance ToJSON Pixels where
  toJSON (Pixels n) = Number $ fromIntegral n

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

data InputConfig = InputConfig { _inputConfigPgConnection :: Text
                     , _inputConfigPgPoolSize             :: Maybe Int
                     , _inputConfigPgTimeout              :: Maybe NominalDiffTime
                     , _inputConfigMapnikInputPlugins     :: Maybe FilePath
                     , _inputConfigPort                   :: Maybe Int
                     , _inputConfigLayers                 :: M.Map Text Layer
                     , _inputConfigTileBuffer             :: Maybe Pixels
                     } deriving (Show, Generic)

makeLenses ''InputConfig

data Config = Config { _configPgConnection       :: Text
                     , _configPgPoolSize         :: Int
                     , _configPgTimeout          :: NominalDiffTime
                     , _configMapnikInputPlugins :: FilePath
                     , _configPort               :: Int
                     , _configLayers             :: M.Map Text Layer
                     , _configTileBuffer         :: Pixels
                     } deriving (Show, Generic)

makeLenses ''Config

emptyInputConfig :: InputConfig
emptyInputConfig = InputConfig "" Nothing Nothing Nothing Nothing (fromList []) Nothing

instance FromJSON InputConfig where
  parseJSON (Object o) =
       InputConfig <$> o .: "db-connection" <*> o .:? "db-pool-size" <*> o .:? "db-timeout" <*>
          o .:? "mapnik-input-plugins" <*> o .:? "port" <*> o .: "layers" <*> (fmap . fmap) Pixels (o .:? "tile-buffer")
  parseJSON _ = Control.Applicative.empty

instance ToJSON Config where
  toJSON c = object
    [
      ("db-connection" .= _configPgConnection c),
      ("db-pool-size" .= _configPgPoolSize c),
      ("db-timeout" .= _configPgTimeout c),
      ("mapnik-input-plugins" .= _configMapnikInputPlugins c),
      ("port" .= _configPort c),
      ("layers" .= _configLayers c),
      ("tile-buffer" .= _configTileBuffer c)
    ]

instance ToJSON InputConfig where
  toJSON ic = object $ catMaybes
    [
      ("db-connection" .=) <$> Just (_inputConfigPgConnection ic),
      ("db-pool-size" .=) <$> _inputConfigPgPoolSize ic,
      ("db-timeout" .=) <$> _inputConfigPgTimeout ic,
      ("mapnik-input-plugins" .=) <$> _inputConfigMapnikInputPlugins ic,
      ("port" .=) <$> _inputConfigPort ic,
      ("layers" .=) <$> Just (_inputConfigLayers ic),
      ("tile-buffer" .=) <$> Just (_inputConfigTileBuffer ic)
    ]

instance ToJSON Layer where
  toJSON l = object
    [  "query" .= _layerQuery l,
       "last-modified" .= _layerLastModified l
    ]

data ServerState = ServerState { _ssPool           :: P.Pool
                               , _ssPluginDir      :: FilePath
                               , _ssConfigFile     :: FilePath
                               , _ssOriginalConfig :: Config
                               , _ssStateLayers    :: STM.Map Text Layer
                               }
makeLenses ''ServerState

ssBuffer :: Lens' ServerState Pixels
ssBuffer = ssOriginalConfig . configTileBuffer

err204 :: ServantErr
err204 = ServantErr { errHTTPCode = 204
                    , errReasonPhrase = "No Content"
                    , errBody = ""
                    , errHeaders = []
                    }

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

newtype TileFeature = TileFeature { unTileFeature :: Value } deriving (Show, Eq)

type GeoJson = M.Map Text Value

mkGeoJSON :: [Value] -> [GJ.Feature]
mkGeoJSON = fmap (x . parseEither parseJSON)
  where
    x = either (\_ -> GJ.Feature Nothing (GJ.GeometryCollection []) Null Nothing) id

instance ToJSON GJ.FeatureCollection where
  toJSON fc = object
    ([ "features" .= GJ.features fc
    , "type" .= String "FeatureCollection"
    ] <> getBbox (GJ.collectionBoundingBox fc))

instance ToJSON GJ.Feature where
  toJSON f = object
    ([ "id" .= GJ.identifier f
    , "type" .= String "Feature"
    , "properties" .= GJ.properties f
    , "geometry" .= GJ.geometry f
    ] <> getBbox (GJ.boundingBox f))

getBbox :: (ToJSON v, KeyValue t) => Maybe v -> [t]
getBbox = maybe [] (\a -> ["bbox" .= a])

instance ToJSON GJ.PointGeometry where
  toJSON (GJ.PointGeometry coords) = toJSON coords

instance ToJSON GJ.LineStringGeometry where
  toJSON (GJ.LineStringGeometry ls) = toJSON ls

instance ToJSON GJ.PolygonGeometry where
  toJSON (GJ.PolygonGeometry ext holes) = toJSON (ext : holes)

instance ToJSON GJ.Geometry where
  toJSON (GJ.Point            po) = object [ "type" .= String "Point", "coordinates" .= po]
  toJSON (GJ.MultiPoint      mpg) = object [ "type" .= String "MultiPoint", "coordinates" .= GJ.points mpg]
  toJSON (GJ.LineString       ls) = object [ "type" .= String "LineString", "coordinates" .= ls]
  toJSON (GJ.MultiLineString mls) = object [ "type" .= String "MultiLineString", "coordinates" .= GJ.lineStrings mls]
  toJSON (GJ.Polygon          pg) = object [ "type" .= String "Polygon", "coordinates" .= pg]
  toJSON (GJ.MultiPolygon    mpg) = object [ "type" .= String "MultiPolygon", "coordinates" .= GJ.polygons mpg]
  toJSON (GJ.GeometryCollection geom) = object [ "type" .= String "GeometryCollection", "geometries" .= fmap toJSON geom ]
