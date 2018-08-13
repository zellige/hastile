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
import           Data.Aeson                   as A
import           Data.Aeson.Types             as AT
import qualified Data.Geometry.Types.Simplify as DGTS
import qualified Data.Geometry.Types.Types    as DGTT
import qualified Data.Geospatial              as DG
import           Data.Map.Strict              as M
import qualified Data.Text                    as T
import qualified Data.Time                    as DT
import           Options.Generic

data LayerRequest = LayerRequest
  {  _newLayerRequestName     :: T.Text
  ,  _newLayerRequestSettings :: LayerSettings
  } deriving (Show, Eq)

newtype LayerRequestList = LayerRequestList [LayerRequest]

instance FromJSON LayerRequestList where
    parseJSON v = (LayerRequestList . fmap (uncurry LayerRequest) . M.toList) Control.Applicative.<$> parseJSON v

data LayerSettings = LayerSettings
  { _lsQuery      :: Text
  , _lsQuantize   :: DGTT.Pixels
  , _lsAlgorithms :: Algorithms
  } deriving (Show, Eq)

instance FromJSON LayerSettings where
  parseJSON = withObject "LayerSettings" $ \o -> LayerSettings
    <$> o .: "query"
    <*> o .: "quantize"
    <*> o .: "simplify"

instance ToJSON LayerSettings where
  toJSON ls = object
    [ "query"    .= _lsQuery ls
    , "quantize" .= _lsQuantize ls
    , "simplify" .= _lsAlgorithms ls
    ]

requestToLayer :: Text -> LayerSettings -> DT.UTCTime -> Layer
requestToLayer layerName (LayerSettings query quantize simplify) time = Layer layerName query time quantize simplify

data Layer = Layer
  { _layerName         :: Text
  , _layerQuery        :: Text
  , _layerLastModified :: DT.UTCTime
  , _layerQuantize     :: DGTT.Pixels
  , _layerAlgorithms   :: Algorithms
  } deriving (Show, Eq, Generic)

data LayerDetails = LayerDetails
  { _layerDetailsQuery        :: Text
  , _layerDetailsLastModified :: DT.UTCTime
  , _layerDetailsQuantize     :: DGTT.Pixels
  , _layerDetailsAlgorithms   :: Algorithms
  } deriving (Show, Eq, Generic)

instance FromJSON LayerDetails where
  parseJSON = withObject "Layer" $ \o -> LayerDetails
    <$> o .: "query"
    <*> o .: "last-modified"
    <*> o .: "quantize"
    <*> o .: "simplify"

instance ToJSON LayerDetails where
  toJSON l = object
    [ "query"         .= _layerDetailsQuery l
    , "last-modified" .= _layerDetailsLastModified l
    , "quantize"      .= _layerDetailsQuantize l
    , "simplify"      .= _layerDetailsAlgorithms l
    ]

layerDetailsToLayer :: Text -> LayerDetails -> Layer
layerDetailsToLayer name LayerDetails{..} = Layer name _layerDetailsQuery _layerDetailsLastModified _layerDetailsQuantize _layerDetailsAlgorithms

layerToLayerDetails :: Layer -> LayerDetails
layerToLayerDetails Layer{..} = LayerDetails _layerQuery _layerLastModified _layerQuantize _layerAlgorithms

-- Zoom dependant simplification algorithms

-- TODO use map Strict

type Algorithms = M.Map DGTT.ZoomLevel DGTS.SimplificationAlgorithm

getAlgorithm :: DGTT.ZoomLevel -> Layer -> DGTS.SimplificationAlgorithm
getAlgorithm z layer = getAlgorithm' z (_layerAlgorithms layer)

getAlgorithm' :: DGTT.ZoomLevel -> Algorithms -> DGTS.SimplificationAlgorithm
getAlgorithm' z algos = case M.lookupGE z algos of
  Nothing        -> DGTS.NoAlgorithm
  Just (_, algo) -> algo

newtype TileFeature = TileFeature
  { unTileFeature :: Value
  } deriving (Show, Eq)

-- Helpers

mkGeoJSON :: [Value] -> [DG.GeoFeature AT.Value]
mkGeoJSON = fmap (x . parseEither parseJSON)
  where
    x = either (\_ -> DG.GeoFeature Nothing (DG.Collection []) Null Nothing) id
