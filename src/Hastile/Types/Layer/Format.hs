{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastile.Types.Layer.Format where

import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as AesonTypes

data LayerFormat = WkbProperties | GeoJson deriving (Eq)

instance Show LayerFormat where
  show WkbProperties = "wkb-properties"
  show GeoJson       = "geojson"

instance Aeson.FromJSON LayerFormat where
  parseJSON = AesonTypes.withText "LayerFormat" $ \case
    "wkb-properties" -> pure WkbProperties
    "geojson"        -> pure GeoJson
    _                -> fail "Unknown layer format"

instance Aeson.ToJSON LayerFormat where
  toJSON algo =
    Aeson.String $ case algo of
      WkbProperties -> "wkb-properties"
      GeoJson       -> "geojson"
