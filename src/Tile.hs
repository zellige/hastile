{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tile ( addBufferToBBox
            , extent
            , googleToBBoxM
            , mkTile
            , BBox (..)
            , Metres (..)
            , Pixels (..)
            , ZoomLevel (..)
            ) where

import qualified Data.Aeson                     as A
import qualified Data.ByteString.Char8          as BS8
import qualified Data.Geometry.MapnikVectorTile as DGM
import qualified Data.Geometry.Types.Types      as DGT
import qualified Data.Geospatial                as DG
import qualified Data.Text                      as T

import           Types

newtype TileCoord  = TileCoord Integer deriving (Show, Eq, Num)
newtype Metres = Metres Double deriving (Show, Eq, Num, Floating, Fractional, Ord)
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

addBufferToBBox :: Pixels -> Pixels -> ZoomLevel -> BBox Metres -> BBox Metres
addBufferToBBox tileSize buffer z (BBox llX llY urX urY) =
  hardLimit $ BBox (llX - bufferM) (llY - bufferM) (urX + bufferM) (urY + bufferM)
  where mPerPx = mPerPxAtZoom earthCircumference tileSize z
        bufferM = mPerPxToM mPerPx buffer
        -- Gots to hard limit the bounds because of buffering
        hardLimit (BBox llX' llY' urX' urY') =
          BBox (max llX' (- maxExtent))
               (max llY' (- maxExtent))
               (min urX' maxExtent)
               (min urY' maxExtent)

googleToBBoxM :: Pixels -> ZoomLevel -> GoogleTileCoords -> BBox Metres
googleToBBoxM tileSize z g =
  flipYs . fmap googleTo3857 $ googleToBBoxPx tileSize g
  where googleTo3857 coord = mPerPxToM mPerPx coord - maxExtent
        mPerPx = mPerPxAtZoom earthCircumference tileSize z
        flipYs (BBox llX llY urX urY) = BBox llX (-llY) urX (-urY)

googleToBBoxPx :: Pixels -> GoogleTileCoords -> BBox Pixels
googleToBBoxPx tileSize (GoogleTileCoords gx gy) =
  ((* tileSize) . fromIntegral) <$> BBox gx (gy + 1) (gx + 1) gy

pixelMaxExtent :: Pixels -> ZoomLevel -> Pixels
pixelMaxExtent tile (ZoomLevel z) = (2 ^ z) * tile

-- At each zoom level we double the number of tiles in both the x and y direction.
-- z = 0: 1 tile, z = 1: 2 * 2 = 4 tiles, z = 3: 4 * 4 = 16 tiles
mPerPxAtZoom :: Metres -> Pixels -> ZoomLevel -> Ratio Metres Pixels
mPerPxAtZoom (Metres m) tile z = Ratio $ m / fromIntegral p
  where (Pixels p) = pixelMaxExtent tile z

mPerPxToM :: Ratio Metres Pixels -> Pixels -> Metres
mPerPxToM (Ratio r) (Pixels p) = Metres $ r * fromIntegral p

mkTile :: T.Text -> Coordinates -> Pixels -> Pixels-> DG.GeoFeatureCollection A.Value -> IO BS8.ByteString
mkTile l zxy buffer quantizePixels geoJson = do
  mvt <- DGM.createMvt config geoJson
  pure $ DGM.encodeMvt mvt
  where
    config = DGT.mkConfig l (_z . _zl $ zxy) (_x . _xy $ zxy, _y . _xy $ zxy) (DGT.Pixels $ _pixels buffer) (DGT.Pixels $ _pixels defaultTileSize) (DGT.Pixels $ _pixels quantizePixels)


