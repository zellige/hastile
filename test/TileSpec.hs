module TileSpec where

import Test.Hspec
-- import Test.QuickCheck

import Tile

-- extentLatLon :: BBox (LatLon Degrees)
-- extentLatLon = BBox (Lon (-180)) (Lat (-85.05112877980659))
--               (Lon 180) (Lat 85.05112877980659)

spec :: Spec
spec = do
  --testGoogleToBBoxPx
  testGoogleToBBoxM
  -- testTileCentreToBBox
  -- testXPxToLon
  -- testYPxToLat
  -- testZxyToBBox

-- testYPxToLat :: Spec
-- testYPxToLat = do
--   describe "Pixels in Y direction at a given zoom level translate to a latitude" $ do
--     it "Returns northern most latitude for y=0 at any zoom level" $
--       property $ \z -> f (ZoomLevel z) 0 == Lat 85.05112877980659
--     it "Returns southern most latitude for y=tileSize at z=0" $
--       f 0 256 `shouldBe` Lat (-85.05112877980659)
--     it "Returns latitude over antarctica for y=524032 at z=11" $
--       f 11 524032 `shouldBe` Lat (-85.035941506574)
--   where f = yPxToLat 256


-- testGoogleToBBoxPx :: Spec
-- testGoogleToBBoxPx = do
--   describe "Can get a bounding box in pixels given google tile coords and a zoom level" $ do
--     it "Returns bounding box for tile at 0, 0 for any tile size" $
--       property $ \tile -> googleToBBoxPx (Pixels tile) (GoogleTileCoords 0 0) == (BBox 0 (Pixels tile) (Pixels tile) 0)
--     it "Returns bounding box for 2, 3" $
--       googleToBBoxPx 256 (GoogleTileCoords 2 3) `shouldBe` BBox 512 1024 768 768

testGoogleToBBoxM :: Spec
testGoogleToBBoxM = do
  describe "Can get a bounding box in 3857 metres from a google tile" $ do
    it "Returns the 3857 extent for zoom level 0" $
      googleToBBoxM 256 0 (GoogleTileCoords 0 0) `shouldBe` extent

-- testTileCentreToBBox :: Spec
-- testTileCentreToBBox = do
--   describe "Translating from z,x,y to a bounding box centred on x,y" $ do
--     it "Returns the map extent of the earth for any box at z=0" $
--       tileCentreToBBox 256 0 0 0 `shouldBe` extentLatLon
--     it "Returns the bounding box for Mitchell, Roma, Augathella at zoom 7" $
--       tileCentreToBBox 256 7 1.64370185624443e7 (-2974317.644632779) `shouldBe`
--         BBoxM (Metres 1.6280475528516259e7) (Metres (-3130860.67856082))
--               (Metres 1.659356159637234e7) (Metres (-2817774.610704738))

-- testXPxToLon :: Spec
-- testXPxToLon = do
--   describe "Pixels in X direction at a given zoom level translate to a longitude" $ do
--     it "Returns -180 for 0 at any zoom level" $
--       property $ \z -> f (ZoomLevel z) 0 == Lon (-180.0)
--     it "Returns lon in Brisbane, Australia for z=11, x=484864" $
--       f 11 484864 `shouldBe` Lon 152.9296875
--   where f = xPxToLon 256

-- testZxyToBBox :: Spec
-- testZxyToBBox = do
--   describe "Translating from Google tile coordinates to lat/lon" $ do
--     it "returns the map extent in lat/lon for any box at z=0" $
--       property $ \z -> zxyToBBox 256 (ZoomLevel z) (GoogleTileCoords 0 0) `shouldBe` extentLatLon
