{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SphericalMercator ( earthRadius
                         , earthCircumference
                         , extent
                         , pxToLatLon
                         , tileCentreToBBox
                         , xPxToLon
                         , yPxToLat
                         , zxyToBBox
                         , BBox (..)
                         , LatLon (..)
                         , Metres (..)
                         , Pixels (..)
                         , ZoomLevel (..)
                         ) where

newtype TileCoord  = TileCoord Integer deriving (Show, Eq, Num)
newtype Metres = Metres Double deriving (Show, Eq, Num, Floating, Fractional)
newtype Pixels = Pixels Double deriving (Show, Eq, Num, Floating, Fractional)
newtype ZoomLevel = ZoomLevel Integer deriving (Show, Eq, Num)
newtype Ratio n d = Ratio Double deriving (Show, Eq, Num, Floating, Fractional)

data LatLon a = Lat Double
              | Lon Double
              deriving (Show, Eq)
data Radians
data Degrees

data GoogleTileCoords = GoogleTileCoords Int Int deriving (Eq, Show)

-- SW and NE points given as W,S,E,N
data BBox a = BBoxM Metres Metres Metres Metres
            | BBoxLLD (LatLon Degrees) (LatLon Degrees) (LatLon Degrees) (LatLon Degrees)
            | BBoxLLR (LatLon Radians) (LatLon Radians) (LatLon Radians) (LatLon Radians)
              deriving (Show, Eq)

earthRadius :: Metres
earthRadius = (Metres 6378137)

earthCircumference :: Metres
earthCircumference = circumferenceM earthRadius

circumferenceM :: Metres -> Metres
circumferenceM (Metres r) = Metres $ r * 2 * pi

extent :: BBox Metres
extent = BBoxM (- earthCircumference / twoM)
               (- earthCircumference / twoM)
               (earthCircumference / twoM)
               (earthCircumference / twoM)
         where twoM = Metres 2

zxyToBBox :: Pixels -> ZoomLevel -> GoogleTileCoords -> BBox (LatLon Degrees)
zxyToBBox _p _z _t = BBoxLLD (Lon 0) (Lat 0) (Lon 0) (Lat 0)

tileCentreToBBox :: Pixels -> ZoomLevel -> Metres -> Metres -> BBox Metres
tileCentreToBBox p z x y = BBoxM (x - halfTileMetres)
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

pxToLatLon :: Pixels -> ZoomLevel -> (Pixels, Pixels) -> (LatLon Degrees, LatLon Degrees)
pxToLatLon tileSize z (x, y) = (xPxToLon tileSize z x, yPxToLat tileSize z y)

xPxToLon :: Pixels -> ZoomLevel -> Pixels -> LatLon Degrees
xPxToLon (Pixels tileSize) (ZoomLevel z) (Pixels x) =
  Lon $ (x * 180) / ((tileSize / 2) * 2 ** fromInteger z) - 180

yPxToLat :: Pixels -> ZoomLevel -> Pixels -> LatLon Degrees
yPxToLat tileSize (ZoomLevel z) y =
  let denom = tileSize / 2 * 2 ** fromInteger z
      (Pixels g) = (-180) * y / denom + pi
   in Lat $ 2 * atanDeg(exp g) - (180 / 2)

atanDeg :: Floating a => a -> a
atanDeg = (/ pi) . (* 180) . atan
