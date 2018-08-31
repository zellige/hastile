{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hastile.Lib.Tile (
              addBufferToBBox
            , extent
            , getBbox
            , googleToBBoxM
            , mkTile
            ) where

import qualified Data.Aeson                     as A
import qualified Data.ByteString.Char8          as BS8
import qualified Data.Geometry.MapnikVectorTile as DGM
import qualified Data.Geometry.Types.Config     as DGTC
import qualified Data.Geometry.Types.Geography  as DGTT
import qualified Data.Geometry.Types.Simplify   as DGTS
import qualified Data.Geospatial                as DG
import qualified Data.Text                      as Text

import qualified Hastile.Types.Config           as Config
import qualified Hastile.Types.Tile             as Tile

addBufferToBBox :: DGTT.Pixels -> DGTT.Pixels -> DGTT.ZoomLevel -> Tile.BBox Tile.Metres -> Tile.BBox Tile.Metres
addBufferToBBox tileSize buffer z (Tile.BBox llX llY urX urY) =
  hardLimit $ Tile.BBox (llX - bufferM) (llY - bufferM) (urX + bufferM) (urY + bufferM)
  where mPerPx = mPerPxAtZoom earthCircumference tileSize z
        bufferM = mPerPxToM mPerPx buffer
        -- Gots to hard limit the bounds because of buffering
        hardLimit (Tile.BBox llX' llY' urX' urY') =
          Tile.BBox (max llX' (- maxExtent))
               (max llY' (- maxExtent))
               (min urX' maxExtent)
               (min urY' maxExtent)

extent :: Tile.BBox Tile.Metres
extent = Tile.BBox (-maxExtent) (-maxExtent) maxExtent maxExtent

earthRadius :: Tile.Metres
earthRadius = Tile.Metres 6378137

maxExtent :: Tile.Metres
maxExtent = pi * earthRadius

earthCircumference :: Tile.Metres
earthCircumference = 2 * maxExtent

getBbox :: DGTT.Pixels -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> Tile.BBox Tile.Metres
getBbox buffer z xy =
  addBufferToBBox Config.defaultTileSize buffer z bboxM
  where
    bboxM = googleToBBoxM Config.defaultTileSize z xy

googleToBBoxM :: DGTT.Pixels -> DGTT.ZoomLevel -> (DGTT.Pixels, DGTT.Pixels) -> Tile.BBox Tile.Metres
googleToBBoxM tileSize z xy =
  flipYs . fmap googleTo3857 $ googleToBBoxPx tileSize xy
  where googleTo3857 coord = mPerPxToM mPerPx coord - maxExtent
        mPerPx = mPerPxAtZoom earthCircumference tileSize z
        flipYs (Tile.BBox llX llY urX urY) = Tile.BBox llX (-llY) urX (-urY)

googleToBBoxPx :: DGTT.Pixels -> (DGTT.Pixels, DGTT.Pixels) -> Tile.BBox DGTT.Pixels
googleToBBoxPx tileSize (x, y) =
  (* tileSize) . fromIntegral <$> Tile.BBox x (y + 1) (x + 1) y

pixelMaxExtent :: DGTT.Pixels -> DGTT.ZoomLevel -> DGTT.Pixels
pixelMaxExtent tile z = (2 ^ z) * tile

newtype Ratio n d = Ratio Double deriving (Show, Eq, Num, Floating, Fractional)

-- At each zoom level we double the number of tiles in both the x and y direction.
-- z = 0: 1 tile, z = 1: 2 * 2 = 4 tiles, z = 3: 4 * 4 = 16 tiles
mPerPxAtZoom :: Tile.Metres -> DGTT.Pixels -> DGTT.ZoomLevel -> Ratio Tile.Metres DGTT.Pixels
mPerPxAtZoom (Tile.Metres m) tile z = Ratio $ m / fromIntegral p
  where p = pixelMaxExtent tile z

mPerPxToM :: Ratio Tile.Metres DGTT.Pixels -> DGTT.Pixels -> Tile.Metres
mPerPxToM (Ratio r) p = Tile.Metres $ r * fromIntegral p

mkTile :: Text.Text -> DGTT.Pixels -> (DGTT.Pixels, DGTT.Pixels) -> DGTT.Pixels -> DGTT.Pixels -> DGTS.SimplificationAlgorithm -> DG.GeoFeatureCollection A.Value -> IO BS8.ByteString
mkTile l z xy buffer quantizePixels algo geoJson = DGM.encodeMvt <$> DGM.createMvt config geoJson
  where
    config = DGTC.mkConfig l z xy buffer Config.defaultTileSize quantizePixels algo
