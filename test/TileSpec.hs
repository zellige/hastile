{-# LANGUAGE OverloadedStrings #-}

module TileSpec where

import           Control.Monad.IO.Class
import qualified Data.ByteString                 as BS
import           Data.Either
import           Data.Map
import           Data.Text                       (Text)
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

readMvtFile = do
  mvt <- BS.readFile "test/integration/19781.mvt"
  let decodeMvt = VT.decode mvt
      decodedMvt = Prelude.head $ rights [decodeMvt]
      layers = VVT._layers . Prelude.head $ rights [VT.tile decodedMvt]
      maybeLayer = Data.Map.lookup "open_traffic_adl" layers
  pure maybeLayer

readLayer = do

-- Test for serialize/deserialize
-- Test for empty queries.
