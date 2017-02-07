{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.ByteString.Lazy as LBS (fromStrict, readFile)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (unpack)
import           System.Environment   (lookupEnv)
import           Test.Tasty
import           Test.Tasty.Golden

import           MapboxVectorTile
import           Tile
import           Types

tileSize :: Pixels
tileSize = 2048

main :: IO ()
main = defaultMain integrationTest

integrationTest :: TestTree
integrationTest =
  let name = "MVT creation"
      golden = "test/integration/mvt.golden"
      geoJsonFile = "test/integration/tile.json"
      layerName = "golden-test"
      (z,x,y) = (14,13464,9727)
      coords = Coordinates z $ GoogleTileCoords x y
      actual = do
        bs <- LBS.readFile geoJsonFile
        let ebs = eitherDecode bs :: Either String GeoJson
            decodeError = (error . ("Unable to decode " <> geoJsonFile <> ": ") <>)
            geoJson = either decodeError id ebs
        pluginDir <- fromMaybe "/usr/local/lib/mapnik/input" <$> lookupEnv "MAPNIK_PLUGINS_DIR"
        et <- fromGeoJSON tileSize geoJson layerName pluginDir coords
        either (error . unpack . ("Failed to create tile: " <>)) (return . fromStrict) et
   in goldenVsString name golden actual
