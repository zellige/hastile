{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tile ( addBufferToBBox
            , extent
            , googleToBBoxM
            , mkTile
            , BBox (..)
            , Metres (..)
            ) where

import qualified Data.Aeson                     as A
import qualified Data.ByteString.Char8          as BS8
import qualified Data.Geometry.MapnikVectorTile as DGM
import qualified Data.Geometry.Types.Simplify   as DGTS
import qualified Data.Geometry.Types.Types      as DGTT
import qualified Data.Geospatial                as DG
import qualified Data.Text                      as T

import qualified Types                          as T

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

addBufferToBBox :: DGTT.Pixels -> DGTT.Pixels -> DGTT.ZoomLevel -> BBox Metres -> BBox Metres
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

googleToBBoxM :: DGTT.Pixels -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> BBox Metres
googleToBBoxM tileSize z xy =
  flipYs . fmap googleTo3857 $ googleToBBoxPx tileSize xy
  where googleTo3857 coord = mPerPxToM mPerPx coord - maxExtent
        mPerPx = mPerPxAtZoom earthCircumference tileSize z
        flipYs (BBox llX llY urX urY) = BBox llX (-llY) urX (-urY)

googleToBBoxPx :: DGTT.Pixels -> (DGTT.Pixels, DGTT.Pixels) -> BBox DGTT.Pixels
googleToBBoxPx tileSize (x, y) =
  (* tileSize) . fromIntegral <$> BBox x (y + 1) (x + 1) y

pixelMaxExtent :: DGTT.Pixels -> DGTT.ZoomLevel -> DGTT.Pixels
pixelMaxExtent tile z = (2 ^ z) * tile

-- At each zoom level we double the number of tiles in both the x and y direction.
-- z = 0: 1 tile, z = 1: 2 * 2 = 4 tiles, z = 3: 4 * 4 = 16 tiles
mPerPxAtZoom :: Metres -> DGTT.Pixels -> DGTT.ZoomLevel -> Ratio Metres DGTT.Pixels
mPerPxAtZoom (Metres m) tile z = Ratio $ m / fromIntegral p
  where p = pixelMaxExtent tile z

mPerPxToM :: Ratio Metres DGTT.Pixels -> DGTT.Pixels -> Metres
mPerPxToM (Ratio r) p = Metres $ r * fromIntegral p

-- mkConfig :: Text -> Pixels -> (Pixels, Pixels) -> Pixels -> Pixels -> Pixels -> Config
mkTile :: T.Text -> DGTT.Pixels -> (DGTT.Pixels, DGTT.Pixels) -> DGTT.Pixels -> DGTT.Pixels -> DGTS.SimplificationAlgorithm -> DG.GeoFeatureCollection A.Value -> IO BS8.ByteString
mkTile l z xy buffer quantizePixels algo geoJson = do
  mvt <- DGM.createMvt config geoJson
  pure $ DGM.encodeMvt mvt
  where
    config = DGTT.mkConfig l z xy buffer T.defaultTileSize quantizePixels algo
