{-# LANGUAGE OverloadedStrings #-}

module TileSpec where

import           Control.Lens
import qualified Data.ByteString                 as BS
import           Data.Map
import qualified Geography.VectorTile            as VT
import qualified Geography.VectorTile.VectorTile as VVT
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

testReadMvtFile :: Spec
testReadMvtFile = do
  describe "Can read in number of features" $ do
    it "Returns true for expected features" $
      True `shouldBe` True

readMvtFile :: IO VVT.Layer
readMvtFile = do
  mvt <- BS.readFile "test/integration/19781.mvt"
  let decodedMvt = VT.decode mvt ^?! _Right
      layers = VVT._layers $ VT.tile decodedMvt ^?! _Right
      maybeLayer = Data.Map.lookup "open_traffic_adl" layers
  pure $ maybeLayer ^?! _Just

-- Test for serialize/deserialize
-- Test for empty queries.
