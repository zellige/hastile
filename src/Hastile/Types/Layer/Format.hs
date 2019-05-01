{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastile.Types.Layer.Format where

import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text        as DataText

data LayerFormat = Source | WkbProperties | GeoJSON deriving (Eq)

instance Show LayerFormat where
  show Source        = "source"
  show WkbProperties = "wkb-properties"
  show GeoJSON       = "geojson"

instance Aeson.FromJSON LayerFormat where
  parseJSON = AesonTypes.withText "LayerFormat" $ \case
    "source" -> pure Source
    "wkb-properties" -> pure WkbProperties
    "geojson"        -> pure GeoJSON
    _                -> fail "Unknown layer format"

instance Aeson.ToJSON LayerFormat where
  toJSON =
    Aeson.String . DataText.pack . show
