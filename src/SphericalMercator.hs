{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SphericalMercator ( earthRadius
                         , earthCircumference
                         , extent
                         , tileCentreToBBox
                         , BBox (..)
                         , Metres (..)
                         , Pixels (..)
                         ) where

-- SW and NE points given as W,S,E,N
data BBox a = BBox a a a a deriving (Show, Eq)

newtype TileCoord  = TileCoord Int deriving (Show, Eq, Num)
newtype Metres = Metres Double deriving (Show, Eq, Num, Floating, Fractional)
newtype Pixels = Pixels Double deriving (Show, Eq, Num, Floating, Fractional)
newtype ZoomLevel = ZoomLevel Int deriving (Show, Eq, Num)
newtype Ratio n d = Ratio Double deriving (Show, Eq, Num, Floating, Fractional)

earthRadius :: Metres
earthRadius = (Metres 6378137)

earthCircumference :: Metres
earthCircumference = circumferenceM earthRadius

circumferenceM :: Metres -> Metres
circumferenceM (Metres r) = Metres $ r * 2 * pi

extent :: BBox Metres
extent = BBox (- earthCircumference / twoM)
              (- earthCircumference / twoM)
              (earthCircumference / twoM)
              (earthCircumference / twoM)
         where twoM = Metres 2

tileCentreToBBox :: Pixels -> ZoomLevel -> Metres -> Metres -> BBox Metres
tileCentreToBBox p z x y = BBox (x - halfTileMetres)
                                (y - halfTileMetres)
                                (x + halfTileMetres)
                                (y + halfTileMetres)
  where mPerPx = mPerPxAtZoom earthCircumference p z
        halfTilePixels = p / Pixels 2
        halfTileMetres = mPerPxToM mPerPx halfTilePixels

-- At each zoom level we double the number of tiles in both the x and y direction.
-- z = 0: 1 tile, z = 1: 2 * 2 = 4 tiles, z = 3: 8 * 8 = 64 tiles
mPerPxAtZoom :: Metres -> Pixels -> ZoomLevel -> Ratio Metres Pixels
mPerPxAtZoom (Metres m) (Pixels p) (ZoomLevel z) =
  Ratio $ m / (2 ** (fromIntegral z) * p)

mPerPxToM :: Ratio Metres Pixels -> Pixels -> Metres
mPerPxToM (Ratio r) (Pixels p) = Metres $ r * p
