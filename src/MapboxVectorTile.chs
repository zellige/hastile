{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

#include "mvt_from_geojson.h"

module MapboxVectorTile (fromGeoJSON) where

import           Data.ByteString.Char8 as BS8
import           Data.ByteString.Lazy.Char8 as LBS8
import           Data.Text as T
import           Foreign
import           Data.Aeson
import           Foreign.C.String (withCString, peekCString)

import           Tile
import           Types

{# pointer *mvtc_return as MvtcReturn foreign finalizer mvtc_free_mvtc_return newtype #}
{# enum mvtc_return_code as MvtcReturnCode {underscoreToCase} deriving (Eq) #}

fromGeoJSON :: Pixels
            -> GeoJson
            -> Text
            -> FilePath
            -> Coordinates
            -> IO (Either Text BS8.ByteString)
fromGeoJSON (Pixels tileSize)
            geoJSON
            layerName
            inputPluginsPath
            zxy =
  withCString (LBS8.unpack $ encode geoJSON) $ \cGeoJSON ->
  withCString (T.unpack layerName) $ \cLayerName ->
  withCString inputPluginsPath $ \cInputPluginsPath -> do
    mvtcReturn <- MvtcReturn <$> (newForeignPtr mvtc_free_mvtc_return =<<
      {# call mvtc_from_geo_json #} (fromInteger tileSize)
                                    cGeoJSON
                                    cLayerName
                                    cInputPluginsPath
                                    (fromInteger . _z $ _zl zxy)
                                    (fromInteger . _x $ _xy zxy)
                                    (fromInteger . _y $ _xy zxy))
    withMvtcReturn mvtcReturn $ \ptr ->
      if ptr == nullPtr
        then return $ Left miserable
        else handleMvtcReturn ptr
  where miserable = "Mapnik vector tile wrapper failed miserably - didn't even allocate a return value!"

handleMvtcReturn :: Ptr MvtcReturn -> IO (Either Text BS8.ByteString)
handleMvtcReturn rPtr = do
    rc <- toEnum . fromIntegral <$> {# call mvtc_get_return_code #} rPtr
    if rc == MvtcSuccess
      then Right <$> getMvtTile rPtr
      else Left . T.pack <$> ({# call mvtc_get_message #} rPtr >>= peekCString)

getMvtTile :: Ptr MvtcReturn -> IO BS8.ByteString
getMvtTile rPtr = do
  cStringSize <- fromIntegral <$> ({# call mvtc_get_mvt_size #} rPtr)
  cString <- {# call mvtc_get_mvt #} rPtr
  packCStringLen (cString, cStringSize)
