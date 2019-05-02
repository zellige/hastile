{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hastile.Types.Layer
  ( Algorithms
  , Layer(..)
  , LayerError(..)
  , LayerRequestList(..)
  , LayerSettings(..)
  , NewLayerRequest(..)
  , layerSecurity
  , layerFormat
  , layerTableName
  , layerQuantize
  , layerAlgorithms
  , layerLastModified
  , lastModifiedFromLayer
  , isModified
  , getAlgorithm
  ) where

import           Control.Applicative           ((<$>))
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as AesonTypes
import qualified Data.Geometry.Types.Config    as TypesConfig
import qualified Data.Geometry.Types.Geography as GeometryTypesGeography
import qualified Data.Map.Strict               as MapStrict
import qualified Data.Maybe                    as DataMaybe
import qualified Data.Text                     as Text
import qualified Data.Time                     as Time
import           Options.Generic               (Generic)

import qualified Hastile.Types.Layer.Format    as LayerFormat
import qualified Hastile.Types.Layer.Security  as LayerSecurity
import qualified Hastile.Types.Time            as LayerTime

data LayerError = LayerNotFound

data NewLayerRequest = NewLayerRequest
  { _newLayerRequestName     :: Text.Text
  , _newLayerRequestSettings :: LayerSettings
  } deriving (Show, Eq)

newtype LayerRequestList = LayerRequestList [NewLayerRequest]

instance Aeson.FromJSON LayerRequestList where
  parseJSON v = LayerRequestList . fmap (uncurry NewLayerRequest) . MapStrict.toList <$> AesonTypes.parseJSON v

data LayerSettings = LayerSettings
  { _layerSecurity     :: Maybe LayerSecurity.LayerSecurity
  , _layerFormat       :: Maybe LayerFormat.LayerFormat
  , _layerTableName    :: Maybe Text.Text
  , _layerQuantize     :: Maybe GeometryTypesGeography.Pixels
  , _layerAlgorithms   :: Maybe Algorithms
  , _layerLastModified :: Maybe Time.UTCTime
  } deriving (Show, Eq)

instance Aeson.FromJSON LayerSettings where
  parseJSON = AesonTypes.withObject "LayerSettings" $ \o -> LayerSettings
    <$> o AesonTypes..:? "security"
    <*> o AesonTypes..:? "format"
    <*> o AesonTypes..:? "table-name"
    <*> o AesonTypes..:? "quantize"
    <*> o AesonTypes..:? "simplify"
    <*> o AesonTypes..:? "last-modified"

instance Aeson.ToJSON LayerSettings where
  toJSON ls = AesonTypes.object $ layerSettingsToPairs ls

layerSettingsToPairs :: LayerSettings -> [AesonTypes.Pair]
layerSettingsToPairs ls =
  [ "security"      AesonTypes..= _layerSecurity ls
  , "format"        AesonTypes..= _layerFormat ls
  , "table-name"    AesonTypes..= _layerTableName ls
  , "quantize"      AesonTypes..= _layerQuantize ls
  , "simplify"      AesonTypes..= _layerAlgorithms ls
  , "last-modified" AesonTypes..= _layerLastModified ls
  ]

data Layer = Layer
  { _layerName     :: Text.Text
  , _layerSettings :: LayerSettings
  } deriving (Show, Eq, Generic)

layerSecurity :: Layer -> LayerSecurity.LayerSecurity
layerSecurity =
  getLayerSetting LayerSecurity.Private _layerSecurity

layerFormat :: Layer -> LayerFormat.LayerFormat
layerFormat =
  getLayerSetting LayerFormat.Source _layerFormat

layerTableName :: Layer -> Text.Text
layerTableName layer@Layer{..} =
  getLayerSetting _layerName _layerTableName layer

layerQuantize :: Layer -> GeometryTypesGeography.Pixels
layerQuantize =
  getLayerSetting 1 _layerQuantize

layerAlgorithms :: Layer -> Algorithms
layerAlgorithms =
  getLayerSetting MapStrict.empty _layerAlgorithms

layerLastModified :: Time.UTCTime -> Layer -> Time.UTCTime
layerLastModified serverStartTime =
  getLayerSetting serverStartTime _layerLastModified

lastModifiedFromLayer :: Time.UTCTime -> Layer -> Text.Text
lastModifiedFromLayer serverStartTime layer =
  LayerTime.lastModified $ layerLastModified serverStartTime layer

isModified :: Time.UTCTime -> Layer -> Maybe Text.Text -> Bool
isModified serverStartTime layer maybeTimeText =
  case maybeTimeText of
    Nothing   -> True
    Just textTime -> isModifiedTime serverStartTime layer $ parseTime textTime
  where parseTime = Time.parseTimeM True Time.defaultTimeLocale isModifiedTimeFormat . Text.unpack
        isModifiedTimeFormat = "%a, %e %b %Y %T GMT"

-- Zoom dependant simplification algorithms

type Algorithms = MapStrict.Map GeometryTypesGeography.ZoomLevel TypesConfig.SimplificationAlgorithm

getAlgorithm :: GeometryTypesGeography.ZoomLevel -> Layer -> TypesConfig.SimplificationAlgorithm
getAlgorithm z layer =
  case MapStrict.lookupGE z $ layerAlgorithms layer of
    Nothing        -> TypesConfig.NoAlgorithm
    Just (_, algo) -> algo

-- Helpers

getLayerSetting :: a -> (LayerSettings -> Maybe a) -> Layer -> a
getLayerSetting _default getter layer =
  DataMaybe.fromMaybe _default . getter $ _layerSettings  layer

isModifiedTime :: Time.UTCTime -> Layer -> Maybe Time.UTCTime -> Bool
isModifiedTime serverStartTime layer mTime =
  case mTime of
    Nothing   -> True
    Just time -> layerLastModified serverStartTime layer > time
