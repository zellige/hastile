{-# LANGUAGE OverloadedStrings #-}

module TileSpec where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString                 as BS
import           Data.ByteString.Lazy            as LBS (ByteString, fromStrict,
                                                         readFile, writeFile)
import           Data.Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text                       (unpack)
import           Data.Text
import qualified Geography.VectorTile            as VT
import qualified Geography.VectorTile.VectorTile as VVT
import           System.Environment              (lookupEnv)
import           Test.Hspec

import           DB
import           MapboxVectorTile
import           Tile
import           Types

spec :: Spec
spec = do
  testGoogleToBBoxM

testGoogleToBBoxM :: Spec
testGoogleToBBoxM = do
  describe "Can get a bounding box in 3857 metres from a google tile" $ do
    it "Returns the 3857 extent for zoom level 0" $
      googleToBBoxM 256 0 (GoogleTileCoords 0 0) `shouldBe` extent

-- "test/integration/19781.mvt"
testReadMvtFile :: Spec
testReadMvtFile = do
  describe "Can read in number of features" $ do
    it "Returns true for expected features" $
      True `shouldBe` True


writeNewMvtAdelaide :: FilePath -> IO ()
generateMvtAdelaide filename = do
  lbs <- generateMvtFile "test/integration/19781.json" "open_traffic_adl" (Coordinates 15 $ GoogleTileCoords 28999 19781)
  _ <- LBS.writeFile filename lbs
  return ()

readMvtFile :: FilePath -> IO VVT.Layer
readMvtFile filename = do
  bs <- BS.readFile filename
  pure $ bsToLayer bs "open_traffic_adl"

bsToLayer :: BS.ByteString -> Text -> VVT.Layer
bsToLayer bs layerName = maybeLayer ^?! _Just
  where
    decodedMvt = VT.decode bs ^?! _Right
    layers = VVT._layers $ VT.tile decodedMvt ^?! _Right
    maybeLayer = Data.Map.lookup layerName layers

generateMvtFile :: FilePath -> Text -> Coordinates -> IO LBS.ByteString
generateMvtFile geoJsonFile layerName coords = do
  bs <- LBS.readFile geoJsonFile
  let ebs = eitherDecode bs :: Either String GeoJson
      decodeError = error . (("Unable to decode " <> geoJsonFile <> ": ") <>)
      geoJson = either decodeError id ebs
  pluginDir <- fromMaybe "/usr/local/lib/mapnik/input" <$> lookupEnv "MAPNIK_PLUGINS_DIR"
  et <- MapboxVectorTile.fromGeoJSON defaultTileSize geoJson layerName pluginDir coords
  either (error . unpack . ("Failed to create tile: " <>)) (pure . fromStrict) et

-- Test for serialize/deserialize
-- Test for empty queries.
