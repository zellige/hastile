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
import qualified Control.Foldl                 as Foldl
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as AesonTypes
import qualified Data.Geometry.Types.Config    as TypesConfig
import qualified Data.Geometry.Types.Geography as GeometryTypesGeography
import qualified Data.Map.Strict               as MapStrict
import qualified Data.Sequence                 as Sequence
import qualified Data.Text                     as Text
import qualified Data.Time                     as Time
import           Options.Generic

import qualified Hastile.Types.Layer.Format    as LayerFormat
import qualified Hastile.Types.Layer.Security  as LayerSecurity
import qualified Hastile.Types.Time            as LayerTime

data LayerError = LayerNotFound

data NewLayerRequest = NewLayerRequest
  {  _newLayerRequestName     :: Text.Text
  ,  _newLayerRequestSettings :: LayerSettings
  } deriving (Show, Eq)

newtype LayerRequestList = LayerRequestList [NewLayerRequest]


foldSeq :: Foldl.Fold a (Sequence.Seq a)
foldSeq = Foldl.Fold step begin done
  where
    begin = Sequence.empty

    step x a = x <> Sequence.singleton a

    done = id

instance Aeson.FromJSON LayerRequestList where
  parseJSON v = (LayerRequestList . fmap (uncurry NewLayerRequest) . MapStrict.toList) Control.Applicative.<$> AesonTypes.parseJSON v

data LayerSettings = LayerSettings
  { _layerSecurity   :: LayerSecurity.LayerSecurity
  , _layerFormat     :: LayerFormat.LayerFormat
  , _layerTableName  :: Text
  , _layerQuantize   :: GeometryTypesGeography.Pixels
  , _layerAlgorithms :: Algorithms
  } deriving (Show, Eq)

instance Aeson.FromJSON LayerSettings where
  parseJSON = AesonTypes.withObject "LayerSettings" $ \o -> LayerSettings
    <$> o AesonTypes..:? "security" AesonTypes..!= LayerSecurity.Private
    <*> o AesonTypes..:  "format"
    <*> o AesonTypes..:  "table-name"
    <*> o AesonTypes..:  "quantize"
    <*> o AesonTypes..:  "simplify"

instance Aeson.ToJSON LayerSettings where
  toJSON ls = AesonTypes.object $ layerSettingsToPairs ls

layerSettingsToPairs :: LayerSettings -> [AesonTypes.Pair]
layerSettingsToPairs ls =
  [ "security"   AesonTypes..= _layerSecurity ls
  , "format"     AesonTypes..= _layerFormat ls
  , "table-name" AesonTypes..= _layerTableName ls
  , "quantize"   AesonTypes..= _layerQuantize ls
  , "simplify"   AesonTypes..= _layerAlgorithms ls
  ]

requestToLayer :: Text -> LayerSettings -> Time.UTCTime -> Layer
requestToLayer layerName layerSettings time = Layer layerName $ LayerDetails layerSettings time

data Layer = Layer
  { _layerName    :: Text
  , _layerDetails :: LayerDetails
  } deriving (Show, Eq, Generic)

data LayerDetails = LayerDetails
  { _layerSettings     :: LayerSettings
  , _layerLastModified :: Time.UTCTime
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON LayerDetails where
  parseJSON = AesonTypes.withObject "LayerDetails" $ \o -> LayerDetails
    <$> AesonTypes.parseJSON (Aeson.Object o)
    <*> o AesonTypes..: "last-modified"

instance Aeson.ToJSON LayerDetails where
  toJSON l = AesonTypes.object $
    "last-modified" AesonTypes..= _layerLastModified l : layerSettingsToPairs (_layerSettings l)

layerToLayerDetails :: Layer -> LayerDetails
layerToLayerDetails Layer{..} = _layerDetails

getLayerDetail :: Layer -> (LayerDetails -> a) -> a
getLayerDetail layer getter =
  getter $ _layerDetails layer

getLayerSetting :: Layer -> (LayerSettings -> a) -> a
getLayerSetting layer getter =
  getter $ _layerSettings $ _layerDetails layer

lastModifiedFromLayer :: Layer -> Text.Text
lastModifiedFromLayer layer = LayerTime.lastModified $ getLayerDetail layer _layerLastModified

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
