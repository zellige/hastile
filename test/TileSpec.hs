module TileSpec where

import           Test.Hspec

import           Tile

spec :: Spec
spec = do
  testGoogleToBBoxM

testGoogleToBBoxM :: Spec
testGoogleToBBoxM = do
  describe "Can get a bounding box in 3857 metres from a google tile" $ do
    it "Returns the 3857 extent for zoom level 0" $
      googleToBBoxM 256 0 (GoogleTileCoords 0 0) `shouldBe` extent

-- Test for serialize/deserialize
-- Test for empty queries.
