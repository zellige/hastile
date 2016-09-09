module SphericalMercatorSpec where

import Test.Hspec
import Test.QuickCheck

import SphericalMercator

-- Lifted data for test cases from http://www.maptiler.org/google-maps-coordinates-tile-bounds-projection/

spec :: Spec
spec = do
  testTileCentreToBBox
  testXPxToLon
  testYPxToLat
  --testZxyToBBox

testTileCentreToBBox :: Spec
testTileCentreToBBox = do
  describe "Translating from z,x,y to a bounding box centred on x,y" $ do
    it "Returns the map extent of the earth for any box at z=0" $
      tileCentreToBBox 256 0 0 0 `shouldBe` extent
    it "Returns the bounding box for Mitchell, Roma, Augathella at zoom 7" $
      tileCentreToBBox 256 7 1.64370185624443e7 (-2974317.644632779) `shouldBe`
        BBoxM (Metres 1.6280475528516259e7) (Metres (-3130860.67856082))
              (Metres 1.659356159637234e7) (Metres (-2817774.610704738))

testXPxToLon :: Spec
testXPxToLon = do
  describe "Pixels in X direction at a given zoom level translate to a longitude" $ do
    it "Returns -180 for 0 at any zoom level" $
      property $ \z -> f (ZoomLevel z) 0 == Lon (-180.0)
    it "Returns lon in Brisbane, Australia for z=11, x=484864" $
      f 11 484864 `shouldBe` Lon 152.9296875
  where f = xPxToLon 256

testYPxToLat :: Spec
testYPxToLat = do
  describe "Pixels in Y direction at a given zoom level translate to a latitude" $ do
    it "Returns northern most latitude for y=0 at any zoom level" $
      property $ \z -> f (ZoomLevel z) 0 == Lat 85.05112877980659
  where f = yPxToLat 256



-- testZxyToBBox :: Spec
-- testZxyToBBox = do
--   describe "Translating from Google tile coordinates to lat/lon" $ do
--     it "returns the map extent in lat/lon for any box at z=0" $
--       zxyToBBox 256 0 0 0 `shouldBe` extent
