{-# LANGUAGE OverloadedStrings #-}

module TileSpec where

import           Control.Lens
import           Data.Aeson                      (eitherDecode)
import qualified Data.ByteString                 as BS (ByteString, readFile)
import qualified Data.ByteString.Lazy            as LBS (ByteString, fromStrict,
                                                         readFile, writeFile)
import           Data.Map                        (lookup)
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text, unpack)
import qualified Geography.VectorTile            as VT
import qualified Geography.VectorTile.VectorTile as VVT
import           System.Environment              (lookupEnv)
import           System.IO                       (hClose)
import           System.IO.Temp                  (withSystemTempFile)
import           Test.Hspec                      (Spec, describe, it, shouldBe)

import           DB                              (defaultTileSize)
import           MapboxVectorTile                (fromGeoJSON)
import           Tile                            (extent, googleToBBoxM)
import           Types

spec :: Spec
spec = do
  testGoogleToBBoxM
  testReadMvtFile

testGoogleToBBoxM :: Spec
testGoogleToBBoxM =
  describe "Can get a bounding box in 3857 metres from a google tile" $
    it "Returns the 3857 extent for zoom level 0" $
      googleToBBoxM 256 0 (GoogleTileCoords 0 0) `shouldBe` extent

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

generateMvtAdelaide :: FilePath -> IO ()
generateMvtAdelaide filename = do
  lbs <- generateMvtFile "test/integration/19781.json" "open_traffic_adl" (Coordinates 15 $ GoogleTileCoords 28999 19781)
  _ <- LBS.writeFile filename lbs
  return ()

generateMvtFile :: FilePath -> Text -> Coordinates -> IO LBS.ByteString
generateMvtFile geoJsonFile layerName coords = do
  bs <- LBS.readFile geoJsonFile
  let ebs = eitherDecode bs :: Either String GeoJson
      decodeError = error . (("Unable to decode " <> geoJsonFile <> ": ") <>)
      geoJson = either decodeError id ebs
  pluginDir <- fromMaybe "/usr/local/lib/mapnik/input" <$> lookupEnv "MAPNIK_PLUGINS_DIR"
  et <- MapboxVectorTile.fromGeoJSON defaultTileSize geoJson layerName pluginDir coords
  either (error . unpack . ("Failed to create tile: " <>)) (pure . LBS.fromStrict) et
