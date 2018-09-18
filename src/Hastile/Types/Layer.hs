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

module Hastile.Types.Layer where

import           Control.Applicative
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types              as AesonTypes
import qualified Data.Geometry.Types.Config    as TypesConfig
import qualified Data.Geometry.Types.Geography as GeometryTypesGeography
import qualified Data.Map.Strict               as MapStrict
import qualified Data.Monoid                   as Monoid
import qualified Data.Text                     as Text
import qualified Data.Time                     as Time
import           Options.Generic

import qualified Hastile.Types.Layer.Format    as LayerFormat
import qualified Hastile.Types.Layer.Security  as LayerSecurity

data LayerError = LayerNotFound

data NewLayerRequest = NewLayerRequest
  {  _newLayerRequestName     :: Text.Text
  ,  _newLayerRequestSettings :: LayerSettings
  } deriving (Show, Eq)

newtype LayerRequestList = LayerRequestList [NewLayerRequest]

instance Aeson.FromJSON LayerRequestList where
  parseJSON v = (LayerRequestList . fmap (uncurry NewLayerRequest) . MapStrict.toList) Control.Applicative.<$> parseJSON v

data LayerSettings = LayerSettings
  { _layerSecurity   :: LayerSecurity.LayerSecurity
  , _layerFormat     :: LayerFormat.LayerFormat
  , _layerTableName  :: Text
  , _layerQuantize   :: GeometryTypesGeography.Pixels
  , _layerAlgorithms :: Algorithms
  } deriving (Show, Eq)

instance Aeson.FromJSON LayerSettings where
  parseJSON = withObject "LayerSettings" $ \o -> LayerSettings
    <$> o .:? "security" .!= LayerSecurity.Private
    <*> o .:  "format"
    <*> o .:  "table-name"
    <*> o .:  "quantize"
    <*> o .:  "simplify"

instance Aeson.ToJSON LayerSettings where
  toJSON ls = object $
    layerSettingsToPairs ls

layerSettingsToPairs :: LayerSettings -> [AesonTypes.Pair]
layerSettingsToPairs ls =
  [ "security"   .= _layerSecurity ls
  , "format"     .= _layerFormat ls
  , "table-name" .= _layerTableName ls
  , "quantize"   .= _layerQuantize ls
  , "simplify"   .= _layerAlgorithms ls
  ]

requestToLayer :: Text -> LayerSettings -> Time.UTCTime -> Layer
requestToLayer layerName layerSettings time = Layer layerName $ LayerDetails time layerSettings

data Layer = Layer
  { _layerName    :: Text
  , _layerDetails :: LayerDetails
  } deriving (Show, Eq, Generic)

data LayerDetails = LayerDetails
  { _layerLastModified :: Time.UTCTime
  , _layerSettings     :: LayerSettings
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON LayerDetails where
  parseJSON = withObject "LayerDetails" $ \o -> LayerDetails
    <$> o .: "last-modified"
    <*> parseJSON (Aeson.Object o)

instance Aeson.ToJSON LayerDetails where
  toJSON l = object $
    "last-modified" .= _layerLastModified l : layerSettingsToPairs (_layerSettings l)

layerToLayerDetails :: Layer -> LayerDetails
layerToLayerDetails Layer{..} = _layerDetails

getLayerDetail :: Layer -> (LayerDetails -> a) -> a
getLayerDetail layer getter =
  getter $ _layerDetails layer

getLayerSetting :: Layer -> (LayerSettings -> a) -> a
getLayerSetting layer getter =
  getter $ _layerSettings $ _layerDetails layer

lastModified :: Layer -> Text.Text
lastModified layer = Text.dropEnd 3 (Text.pack rfc822Str) Monoid.<> "GMT"
       where rfc822Str = Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat $ getLayerDetail layer _layerLastModified

parseIfModifiedSince :: Text.Text -> Maybe Time.UTCTime
parseIfModifiedSince t = Time.parseTimeM True Time.defaultTimeLocale "%a, %e %b %Y %T GMT" $ Text.unpack t

isModifiedTime :: Layer -> Maybe Time.UTCTime -> Bool
isModifiedTime layer mTime =
  case mTime of
    Nothing   -> True
    Just time -> getLayerDetail layer _layerLastModified > time

isModified :: Layer -> Maybe Text.Text -> Bool
isModified layer mText =
  case mText of
    Nothing   -> True
    Just text -> isModifiedTime layer $ parseIfModifiedSince text

-- Zoom dependant simplification algorithms

-- TODO use map Strict

type Algorithms = MapStrict.Map GeometryTypesGeography.ZoomLevel TypesConfig.SimplificationAlgorithm

getAlgorithm :: GeometryTypesGeography.ZoomLevel -> Layer -> TypesConfig.SimplificationAlgorithm
getAlgorithm z layer = getAlgorithm' z $ getLayerSetting layer _layerAlgorithms

getAlgorithm' :: GeometryTypesGeography.ZoomLevel -> Algorithms -> TypesConfig.SimplificationAlgorithm
getAlgorithm' z algos = case MapStrict.lookupGE z algos of
  Nothing        -> TypesConfig.NoAlgorithm
  Just (_, algo) -> algo
