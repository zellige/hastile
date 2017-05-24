{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tile ( extent
            , googleToBBoxM
            , BBox (..)
            , Metres (..)
            , Pixels (..)
            , ZoomLevel (..)
            ) where

import           Types

newtype TileCoord  = TileCoord Integer deriving (Show, Eq, Num)
newtype Metres = Metres Double deriving (Show, Eq, Num, Floating, Fractional)
newtype Ratio n d = Ratio Double deriving (Show, Eq, Num, Floating, Fractional)

data LatLon a = Lat Double
              | Lon Double
              deriving (Show, Eq)

-- SW and NE points given as W,S,E,N
data BBox a = BBox a a a a deriving (Show, Eq, Functor)

earthRadius :: Metres
earthRadius = Metres 6378137

maxExtent :: Metres
maxExtent = pi * earthRadius

earthCircumference :: Metres
earthCircumference = 2 * maxExtent

extent :: BBox Metres
extent = BBox (-maxExtent) (-maxExtent) maxExtent maxExtent

googleToBBoxM :: Pixels -> Pixels -> ZoomLevel -> GoogleTileCoords -> BBox Metres
googleToBBoxM tileSize buffer z g =
  flipYs . fmap (flip (-) maxExtent . mPerPxToM mPerPx) $ googleToBBoxPx tileSize buffer g
  where mPerPx = mPerPxAtZoom earthCircumference tileSize z
        flipYs (BBox llX llY urX urY) = BBox llX (-llY) urX (-urY)

googleToBBoxPx :: Pixels -> Pixels -> GoogleTileCoords -> BBox Pixels
googleToBBoxPx tileSize buffer (GoogleTileCoords gx gy) =
  fmap ((+ buffer) . (* tileSize) . fromIntegral) $ BBox gx (gy + 1) (gx + 1) gy

pixelMaxExtent :: Pixels -> ZoomLevel -> Pixels
pixelMaxExtent tile (ZoomLevel z) = (2 ^ z) * tile

-- At each zoom level we double the number of tiles in both the x and y direction.
-- z = 0: 1 tile, z = 1: 2 * 2 = 4 tiles, z = 3: 4 * 4 = 16 tiles
mPerPxAtZoom :: Metres -> Pixels -> ZoomLevel -> Ratio Metres Pixels
mPerPxAtZoom (Metres m) tile z = Ratio $ m / fromIntegral p
  where (Pixels p) = pixelMaxExtent tile z

mPerPxToM :: Ratio Metres Pixels -> Pixels -> Metres
mPerPxToM (Ratio r) (Pixels p) = Metres $ r * fromIntegral p
