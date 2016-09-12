{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SphericalMercator ( -- earthRadius
                         -- , earthCircumference
                         extent
                         --  bingYToLat
                         -- , pxToLatLon
                         -- , tileCentreToBBox
                         -- , xPxToLon
                         -- , yPxToLat
                         -- , zxyToBBox
                         , googleToBBoxM
                         , googleToBBoxPx
                         , BBox (..)
                         , Degrees
                         , GoogleTileCoords (..)
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

data Degrees

data GoogleTileCoords = GoogleTileCoords Integer Integer deriving (Eq, Show)

-- SW and NE points given as W,S,E,N
data BBox a = BBox a a a a deriving (Show, Eq)

earthRadius :: Metres
earthRadius = (Metres 6378137)

maxExtent :: Metres
maxExtent = pi * earthRadius

earthCircumference :: Metres
earthCircumference = 2 * maxExtent

extent :: BBox Metres
extent = BBox (-maxExtent) (-maxExtent) maxExtent maxExtent

-- zxyToBBox :: Pixels -> ZoomLevel -> GoogleTileCoords -> BBox (LatLon Degrees)
-- zxyToBBox tileSize z t = BBox (xToLon w) (yToLat s) (xToLon e) (yToLat n)
--   where (BBox w s e n) = googleToPx tileSize t
--         xToLon = xPxToLon tileSize z
--         yToLat = yPxToLat tileSize z

googleToBBoxM :: Pixels -> ZoomLevel -> GoogleTileCoords -> BBox Metres
googleToBBoxM tileSize z g =
  flipYs . transformBBox (flip (-) maxExtent . (mPerPxToM mPerPx)) $ googleToBBoxPx tileSize g
  where mPerPx = mPerPxAtZoom earthCircumference tileSize z
        flipYs (BBox llX llY urX urY) = BBox llX (-llY) urX (-urY)

transformBBox :: (a -> b) -> BBox a -> BBox b
transformBBox f (BBox llX llY urX urY) = BBox (f llX) (f llY) (f urX) (f urY)

googleToBBoxPx :: Pixels ->  GoogleTileCoords -> BBox Pixels
googleToBBoxPx tileSize (GoogleTileCoords gx gy) =
  transformBBox ((* tileSize) . fromInteger) $ BBox gx (gy + 1) (gx + 1) gy

pixelMaxExtent :: Pixels -> ZoomLevel -> Pixels
pixelMaxExtent tile (ZoomLevel z) = 2 ** fromInteger z * tile

-- tileCentreToBBox :: Pixels -> ZoomLevel -> Metres -> Metres -> BBox Metres
-- tileCentreToBBox p z x y = BBox (x - halfTileMetres)
--                                 (y - halfTileMetres)
--                                 (x + halfTileMetres)
--                                 (y + halfTileMetres)
--   where mPerPx = mPerPxAtZoom earthCircumference p z
--         halfTilePixels = p / Pixels 2
--         halfTileMetres = mPerPxToM mPerPx halfTilePixels

-- At each zoom level we double the number of tiles in both the x and y direction.
-- z = 0: 1 tile, z = 1: 2 * 2 = 4 tiles, z = 3: 8 * 8 = 64 tiles
mPerPxAtZoom :: Metres -> Pixels -> ZoomLevel -> Ratio Metres Pixels
mPerPxAtZoom (Metres m) tile z = Ratio $ m / p
  where (Pixels p) = pixelMaxExtent tile z

mPerPxToM :: Ratio Metres Pixels -> Pixels -> Metres
mPerPxToM (Ratio r) (Pixels p) = Metres $ r * p

-- pxToLatLon :: Pixels -> ZoomLevel -> (Pixels, Pixels) -> (LatLon Degrees, LatLon Degrees)
-- pxToLatLon tileSize z (x, y) = (xPxToLon tileSize z x, yPxToLat tileSize z y)

-- xPxToLon :: Pixels -> ZoomLevel -> Pixels -> LatLon Degrees
-- xPxToLon (Pixels tileSize) (ZoomLevel z) (Pixels x) =
--   Lon $ (x * 180) / ((tileSize / 2) * 2 ** fromInteger z) - 180

-- yPxToLat :: Pixels -> ZoomLevel -> Pixels -> LatLon Degrees
-- yPxToLat tileSize (ZoomLevel z) y =
--   let denom = tileSize / 2 * 2 ** fromInteger z
--       (Pixels g) = (-180) * y / denom + pi
--    in Lat $ 2 * atanDeg(exp g) - (180 / 2)

-- atanDeg :: Floating a => a -> a
-- atanDeg = (/ pi) . (* 180) . atan

-- bingYToLat :: Integer -> Integer -> Double
-- bingYToLat yPx zoom =
--   0 - 360 * atan (exp ((-y) * 2 * pi)) / pi
--   where mapSize = 2.0  ** fromInteger zoom * 256 :: Double
--         y       = 0.5 - fromInteger yPx / mapSize
