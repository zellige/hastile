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
import           Data.Aeson.Types              as AT
import qualified Data.Geometry.Types.Geography as DGTT
import qualified Data.Geometry.Types.Simplify  as DGTS
import           Data.Map.Strict               as M
import           Data.Monoid
import qualified Data.Text                     as Text
import qualified Data.Time                     as DT
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
  parseJSON v = (LayerRequestList . fmap (uncurry NewLayerRequest) . M.toList) Control.Applicative.<$> parseJSON v

data LayerSettings = LayerSettings
  { _layerSecurity   :: LayerSecurity.LayerSecurity
  , _layerFormat     :: LayerFormat.LayerFormat
  , _layerTableName  :: Text
  , _layerQuantize   :: DGTT.Pixels
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

layerSettingsToPairs :: LayerSettings -> [AT.Pair]
layerSettingsToPairs ls =
  [ "security"   .= _layerSecurity ls
  , "format"     .= _layerFormat ls
  , "table-name" .= _layerTableName ls
  , "quantize"   .= _layerQuantize ls
  , "simplify"   .= _layerAlgorithms ls
  ]

requestToLayer :: Text -> LayerSettings -> DT.UTCTime -> Layer
requestToLayer layerName layerSettings time = Layer layerName $ LayerDetails time layerSettings

data Layer = Layer
  { _layerName    :: Text
  , _layerDetails :: LayerDetails
  } deriving (Show, Eq, Generic)

data LayerDetails = LayerDetails
  { _layerLastModified :: DT.UTCTime
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
lastModified layer = Text.dropEnd 3 (Text.pack rfc822Str) <> "GMT"
       where rfc822Str = DT.formatTime DT.defaultTimeLocale DT.rfc822DateFormat $ getLayerDetail layer _layerLastModified

parseIfModifiedSince :: Text.Text -> Maybe DT.UTCTime
parseIfModifiedSince t = DT.parseTimeM True DT.defaultTimeLocale "%a, %e %b %Y %T GMT" $ Text.unpack t

isModifiedTime :: Layer -> Maybe DT.UTCTime -> Bool
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

type Algorithms = M.Map DGTT.ZoomLevel DGTS.SimplificationAlgorithm

getAlgorithm :: DGTT.ZoomLevel -> Layer -> DGTS.SimplificationAlgorithm
getAlgorithm z layer = getAlgorithm' z $ getLayerSetting layer _layerAlgorithms

getAlgorithm' :: DGTT.ZoomLevel -> Algorithms -> DGTS.SimplificationAlgorithm
getAlgorithm' z algos = case M.lookupGE z algos of
  Nothing        -> DGTS.NoAlgorithm
  Just (_, algo) -> algo
