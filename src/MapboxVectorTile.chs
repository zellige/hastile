{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

#include "mvt_from_geojson.h"

module MapboxVectorTile (fromGeoJSON) where

import           Data.ByteString.Char8 as BS8
import           Data.Text as T
import           Foreign
-- import           Foreign.C.Types (CInt, CChar)
import           Foreign.C.String (withCString, peekCString)

import           Tile

{# pointer *mvtc_return as MvtcReturn foreign finalizer mvtc_free_mvtc_return newtype #}
{# enum mvtc_return_code as MvtcReturnCode {underscoreToCase} deriving (Eq) #}

fromGeoJSON :: Pixels
            -> ByteString
            -> Text
            -> FilePath
            -> ZoomLevel
            -> GoogleTileCoords
            -> IO (Either Text ByteString)
fromGeoJSON (Pixels tileSize)
            geoJSON
            layerName
            inputPluginsPath
            (ZoomLevel z)
            (GoogleTileCoords x y) =
  withCString (BS8.unpack geoJSON) $ \cGeoJSON ->
  withCString (T.unpack layerName) $ \cLayerName ->
  withCString inputPluginsPath $ \cInputPluginsPath -> do
    mvtcReturn <- MvtcReturn <$> (newForeignPtr mvtc_free_mvtc_return =<<
      {# call mvtc_from_geo_json #} (fromInteger tileSize)
                                    cGeoJSON
                                    cLayerName
                                    cInputPluginsPath
                                    (fromInteger z)
                                    (fromInteger x)
                                    (fromInteger y))
    withMvtcReturn mvtcReturn $ \ptr ->
      if ptr == nullPtr
        then return $ Left miserable
        else handleMvtcReturn ptr
  where miserable = "Mapnik vector tile wrapper failed miserably - didn't even allocate a return value!"

handleMvtcReturn :: Ptr MvtcReturn -> IO (Either Text ByteString)
handleMvtcReturn rPtr = do
    rc <- toEnum . fromIntegral <$> {# call mvtc_get_return_code #} rPtr
    if rc == MvtcSuccess
      then Right <$> ({# call mvtc_get_mvt #} rPtr >>= packCString)
      else Left . T.pack <$> ({# call mvtc_get_message #} rPtr >>= peekCString)
