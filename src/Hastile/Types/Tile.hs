{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Hastile.Types.Tile where

import qualified Data.Functor.Contravariant    as Contravariant
import qualified Data.Geometry.Types.Geography as GeometryTypesGeography
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as Text
import qualified Hasql.Encoders                as HasqlEncoders

import qualified Hastile.Types.Config          as Config

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

data TileCenter = TileCenter
   { _tileCenterLongitude :: Double
   , _tileCenterLatitude  :: Double
   , _tileCenterZoom      :: Int
   }

data Tile = Tile
  { _tileStyleVersion :: Text.Text
  , _tileName         :: Maybe Text.Text
  , _tileDescription  :: Maybe Text.Text
  , _tileVersion      :: Maybe Text.Text
  , _tileAttribution  :: Maybe Text.Text
  , _tileTemplate     :: Maybe Text.Text
  , _tileLegend       :: Maybe Text.Text
  , _tileScheme       :: Maybe TileScheme
  -- This is dumb
  , _tileTiles        :: [Text.Text]
  , _tileGrids        :: Maybe [Text.Text]
  , _tileData         :: Maybe [Text.Text]
  , _tileMinZoom      :: Maybe GeometryTypesGeography.ZoomLevel
  , _tileMaxZoom      :: Maybe GeometryTypesGeography.ZoomLevel
  , _tileBoundingBox  :: Maybe GeometryTypesGeography.BoundingBox
  , _tileCenter       :: Maybe TileCenter
  }

fromConfig :: Config.Config -> Text.Text -> Tile
fromConfig Config.Config{..} layerName =
  Tile "2.2.0" (Just layerName) Nothing (Just "1.0.0") Nothing Nothing Nothing (Just TileSchemeXyz) [tileUrl] Nothing Nothing Nothing Nothing Nothing Nothing
  where
    tileUrl = _configProtocolHost <> ":" <> Text.pack (show _configPort) <> "/" <> layerName <> "/{z}/{x}/{y}.mvt"
