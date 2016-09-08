module SphericalMercatorSpec where

import Test.Hspec

import SphericalMercator

spec :: Spec
spec = testZxyToBbox

testZxyToBbox :: Spec
testZxyToBbox = do
  describe "Translating from z,x,y to a bounding box in the spherical mercator works" $ do
    it "Returns the map extent of the earth for any box at z=0" $
      tileCentreToBBox 256 0 0 0 `shouldBe` extent
    it "Returns the bounding box for Mitchell, Roma, Augathella at zoom 7" $
      tileCentreToBBox 256 7 1.64370185624443e7 (-2974317.644632779) `shouldBe`
        BBox (Metres 1.6280475528516259e7) (Metres (-3130860.67856082))
             (Metres 1.659356159637234e7) (Metres (-2817774.610704738))
