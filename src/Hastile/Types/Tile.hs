{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Hastile.Types.Tile where

import qualified Data.Aeson                    as Aeson
import qualified Data.Functor.Contravariant    as Contravariant
import qualified Data.Geometry.Types.Geography as GeometryTypesGeography
import qualified Data.Maybe                    as Maybe
import           Data.Monoid                   ((<>))
import qualified Data.Scientific               as Scientific
import qualified Data.Text                     as Text
import qualified Data.Vector                   as Vector
import qualified Hasql.Encoders                as HasqlEncoders

import qualified Hastile.Types.Config          as Config
import qualified Hastile.Types.Layer           as Layer

-- SW and NE points given as W,S,E,N
data BBox a = BBox
  { _bboxLlx :: a
  , _bboxLly :: a
  , _bboxUrx :: a
  , _bboxUry :: a
  } deriving (Show, Eq, Functor)

newtype Metres = Metres Double deriving (Show, Eq, Num, Floating, Fractional, Ord)

metreValue :: HasqlEncoders.Value Metres
metreValue =
  Contravariant.contramap metreTodouble HasqlEncoders.float8
  where
    metreTodouble (Metres double) = double

bboxEncoder :: HasqlEncoders.Params (BBox Metres)
bboxEncoder =
  Contravariant.contramap _bboxLlx (HasqlEncoders.param metreValue)
  <> Contravariant.contramap _bboxLly (HasqlEncoders.param metreValue)
  <> Contravariant.contramap _bboxUrx (HasqlEncoders.param metreValue)
  <> Contravariant.contramap _bboxUry (HasqlEncoders.param metreValue)

data TileScheme = TileSchemeXyz | TileSchemeTms
  deriving Show

instance Aeson.ToJSON TileScheme where
  toJSON TileSchemeXyz = Aeson.String "xyz"
  toJSON TileSchemeTms = Aeson.String "tms"

data TileCenter = TileCenter
   { _tileCenterLongitude :: Double
   , _tileCenterLatitude  :: Double
   , _tileCenterZoom      :: Int
   }

instance Aeson.ToJSON TileCenter where
  toJSON TileCenter{..} = Aeson.Array $ Vector.fromList [doubleToNumber _tileCenterLongitude, doubleToNumber _tileCenterLatitude, intToNumber _tileCenterZoom]
    where
      doubleToNumber = Aeson.Number . Scientific.fromFloatDigits
      intToNumber i = Aeson.Number (fromInteger $ toInteger i :: Scientific.Scientific)

data Tile = Tile
  { _tileStyleVersion :: Text.Text
  , _tileName         :: Maybe Text.Text
  , _tileDescription  :: Maybe Text.Text
  , _tileVersion      :: Maybe Text.Text
  , _tileAttribution  :: Maybe Text.Text
  , _tileTemplate     :: Maybe Text.Text
  , _tileLegend       :: Maybe Text.Text
  , _tileScheme       :: Maybe TileScheme
  , _tileTiles        :: [Text.Text]
  , _tileGrids        :: Maybe [Text.Text]
  , _tileData         :: Maybe [Text.Text]
  , _tileMinZoom      :: Maybe GeometryTypesGeography.ZoomLevel
  , _tileMaxZoom      :: Maybe GeometryTypesGeography.ZoomLevel
  , _tileBoundingBox  :: Maybe GeometryTypesGeography.BoundingBox
  , _tileCenter       :: Maybe TileCenter
  }

instance Aeson.ToJSON Tile where
  toJSON tile = Aeson.object $
    [ "tilejson"  Aeson..= _tileStyleVersion tile
    , "tiles" Aeson..= _tileTiles tile
    ] ++
   Maybe.catMaybes
    [ ("name"  Aeson..=)           <$> _tileName tile
    , ("description"  Aeson..=)    <$> _tileDescription tile
    , ("version"  Aeson..=)        <$> _tileVersion tile
    , ("attribution"  Aeson..=)    <$> _tileAttribution tile
    , ("template"  Aeson..=)       <$> _tileTemplate tile
    , ("legend"  Aeson..=)         <$> _tileLegend tile
    , ("scheme"  Aeson..=)         <$> _tileScheme tile
    , ("grids"  Aeson..=)          <$> _tileGrids tile
    , ("data"  Aeson..=)           <$> _tileData tile
    , ("minzoom"  Aeson..=)        <$> _tileMinZoom tile
    , ("maxzoom"  Aeson..=)        <$> _tileMaxZoom tile
    , ("bounds"  Aeson..=)         <$> _tileBoundingBox tile
    , ("center"  Aeson..=)         <$> _tileCenter tile
    ]

fromConfig :: Config.Config -> Layer.Layer -> Tile
fromConfig Config.Config{..} layer@Layer.Layer{..} =
  Tile
    { _tileStyleVersion = "2.2.0"
    , _tileName         = Just _layerName
    , _tileDescription  = Nothing
    , _tileVersion      = Just "1.0.0"
    , _tileAttribution  = Nothing
    , _tileTemplate     = Nothing
    , _tileLegend       = Nothing
    , _tileScheme       = Just TileSchemeXyz
    , _tileTiles        = [tileUrl]
    , _tileGrids        = Nothing
    , _tileData         = Nothing
    , _tileMinZoom      = Just $ Layer.layerMinZoom layer
    , _tileMaxZoom      = Just $ Layer.layerMaxZoom layer
    , _tileBoundingBox  = Layer._layerBounds _layerSettings
    , _tileCenter       = Nothing
    }
  where
    tileUrl = _configProtocolHost <> ":" <> Text.pack (show _configPort) <> "/" <> _layerName <> "/{z}/{x}/{y}.mvt"
