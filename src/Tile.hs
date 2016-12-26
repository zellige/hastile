{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tile ( -- earthRadius
            -- , earthCircumference
            extent
            , googleToBBoxM
            -- , googleToBBoxPx
            -- , tileCentreToBBox
            -- , xPxToLon
            -- , yPxToLat
            -- , zxyToBBox
            , BBox (..)
            -- , Degrees
            , GoogleTileCoords (..)
            -- , LatLon (..)
            , Metres (..)
            , Pixels (..)
            , ZoomLevel (..)
            ) where

newtype TileCoord  = TileCoord Integer deriving (Show, Eq, Num)
newtype Metres = Metres Double deriving (Show, Eq, Num, Floating, Fractional)
newtype Pixels = Pixels Integer deriving (Show, Eq, Num)
newtype ZoomLevel = ZoomLevel Integer deriving (Show, Eq, Num)
newtype Ratio n d = Ratio Double deriving (Show, Eq, Num, Floating, Fractional)

data LatLon a = Lat Double
              | Lon Double
              deriving (Show, Eq)

-- data Degrees

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

googleToBBoxM :: Pixels -> ZoomLevel -> GoogleTileCoords -> BBox Metres
googleToBBoxM tileSize z g =
  flipYs . transformBBox (flip (-) maxExtent . (mPerPxToM mPerPx)) $ googleToBBoxPx tileSize g
  where mPerPx = mPerPxAtZoom earthCircumference tileSize z
        flipYs (BBox llX llY urX urY) = BBox llX (-llY) urX (-urY)

transformBBox :: (a -> b) -> BBox a -> BBox b
transformBBox f (BBox llX llY urX urY) = BBox (f llX) (f llY) (f urX) (f urY)

googleToBBoxPx :: Pixels ->  GoogleTileCoords -> BBox Pixels
googleToBBoxPx tileSize (GoogleTileCoords gx gy) =
  transformBBox ((* tileSize) . fromIntegral) $ BBox gx (gy + 1) (gx + 1) gy

pixelMaxExtent :: Pixels -> ZoomLevel -> Pixels
pixelMaxExtent tile (ZoomLevel z) = (2 ^ z) * tile

-- At each zoom level we double the number of tiles in both the x and y direction.
-- z = 0: 1 tile, z = 1: 2 * 2 = 4 tiles, z = 3: 8 * 8 = 64 tiles
mPerPxAtZoom :: Metres -> Pixels -> ZoomLevel -> Ratio Metres Pixels
mPerPxAtZoom (Metres m) tile z = Ratio $ m / fromIntegral p
  where (Pixels p) = pixelMaxExtent tile z

mPerPxToM :: Ratio Metres Pixels -> Pixels -> Metres
mPerPxToM (Ratio r) (Pixels p) = Metres $ r * fromIntegral p

-- zxyToBBox :: Pixels -> ZoomLevel -> GoogleTileCoords -> BBox (LatLon Degrees)
-- zxyToBBox tileSize z t = BBox (xToLon w) (yToLat s) (xToLon e) (yToLat n)
--   where (BBox w s e n) = googleToBBoxPx tileSize t
--         xToLon = xPxToLon tileSize z
--         yToLat = yPxToLat tileSize z


-- tileCentreToBBox :: Pixels -> ZoomLevel -> Metres -> Metres -> BBox Metres
-- tileCentreToBBox px@(Pixels p) z x y =
--   BBox (x - halfTileMetres)
--        (y - halfTileMetres)
--        (x + halfTileMetres)
--        (y + halfTileMetres)
--   where mPerPx = mPerPxAtZoom earthCircumference px z
--         halfTilePixels = Pixels $ p `div` 2
--         halfTileMetres = mPerPxToM mPerPx halfTilePixels

-- xPxToLon :: Pixels -> ZoomLevel -> Pixels -> LatLon Degrees
-- xPxToLon (Pixels tileSize) (ZoomLevel z) (Pixels x) =
--   Lon $ (fromIntegral x * 180) / ((fromIntegral tileSize / 2) * 2 ** fromIntegral z) - 180

-- yPxToLat :: Pixels -> ZoomLevel -> Pixels -> LatLon Degrees
-- yPxToLat (Pixels tileSize) (ZoomLevel z) (Pixels y) =
--   let denom = (fromIntegral tileSize) ** (fromIntegral z)
--       g = ((-180) * fromIntegral y / denom) + pi
--    in Lat $ ((2 *) . atanDeg . exp $ g) - (180 / 2)

-- atanDeg :: Floating a => a -> a
-- atanDeg = (/ pi) . (* 180) . atan
