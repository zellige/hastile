{-# LANGUAGE OverloadedStrings #-}

module Hastile.Lib.TileSpec where

import           Control.Lens
import qualified Data.ByteString                as BS (ByteString, readFile)
import qualified Data.ByteString.Lazy           as LBS (ByteString, fromStrict,
                                                        writeFile)
import qualified Data.Geometry.MapnikVectorTile as MVT
import qualified Data.Geometry.Types.Geography  as DGT
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import qualified Geography.VectorTile           as VT
import           System.IO                      (hClose)
import           System.IO.Temp                 (withSystemTempFile)
import           Test.Hspec                     (Spec, describe, it, shouldBe)


import qualified Data.Geometry.Types.Config     as TypesConfig
import           Hastile.Lib.Tile               (addBufferToBBox, extent,
                                                 googleToBBoxM, mkTile)
import           Hastile.Types.Tile             (BBox (..))

spec :: Spec
spec = do
  testGoogleToBBoxM
  testBufferedBoundingBox
  testReadMvtFile

testGoogleToBBoxM :: Spec
testGoogleToBBoxM =
  describe "googleToBBoxM" $
    it "Returns the 3857 extent for zoom level 0" $
      googleToBBoxM 256 0 (0, 0) `shouldBe` extent

-- TODO: Let's stop writing crappy tests and write properties
testBufferedBoundingBox :: Spec
testBufferedBoundingBox =
  describe "addBufferToBBox" $ do
    it "Hard limits to 3857 extent" $
      let bbox = googleToBBoxM 256 0 (0, 0)
          buffered = addBufferToBBox 256 128 0 bbox
       in buffered `shouldBe` extent
    it "Adding a buffer does what it should" $
      let bbox@(BBox llX llY urX urY) = googleToBBoxM 256 2 (1, 1)
          (BBox llX' llY' urX' urY') = addBufferToBBox 256 128 2 bbox
       in and [llX' < llX , llY' < llY , urX' > urX , urY' > urY] `shouldBe` True

testReadMvtFile :: Spec
testReadMvtFile =
  describe "Can read in number of features" $
    it "Returns true for expected features" $ withSystemTempFile "tile" $ \f h -> do
      hClose h
      generateMvtAdelaide f
      expectedLayer <- readMvtFile "test/integration/19781.mvt"
      newLayer <- readMvtFile f
      newLayer `shouldBe` expectedLayer

-- TODO - Test for empty queries.
-- TODO - Implement fold for comparison when fails

readMvtFile :: FilePath -> IO VT.Layer
readMvtFile filename = do
  bs <- BS.readFile filename
  pure $ bsToLayer bs "open_traffic_adl"

bsToLayer :: BS.ByteString -> LBS.ByteString -> VT.Layer
bsToLayer bs layerName = maybeLayer ^?! _Just
  where
    layers = VT._layers $ VT.tile bs ^?! _Right
    maybeLayer = HM.lookup layerName layers

generateMvtAdelaide :: FilePath -> IO ()
generateMvtAdelaide filename = do
  lbs <- generateMvtFile "test/integration/19781.json" "open_traffic_adl" 15 (28999, 19781)
  _ <- LBS.writeFile filename lbs
  pure ()

generateMvtFile :: FilePath -> T.Text -> DGT.ZoomLevel -> (DGT.Pixels, DGT.Pixels) -> IO LBS.ByteString
generateMvtFile geoJsonFile layerName z xy = do
  mvt <- MVT.readGeoJson geoJsonFile
  x <- mkTile layerName z xy 128 1 TypesConfig.NoAlgorithm mvt
  pure $ LBS.fromStrict x

