{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hastile.Types.Config where

import           Control.Lens                  (makeLenses)
import qualified Data.Aeson                    as Aeson
import qualified Data.Geometry.Types.Geography as DGTT
import qualified Data.Map.Strict               as MapStrict
import           Data.Maybe                    (catMaybes)
import qualified Data.Time                     as Time
import           Options.Generic

import qualified Hastile.Types.Layer           as Layer

data InputConfig = InputConfig
  { _inputConfigPgConnection       :: Text
  , _inputConfigPgPoolSize         :: Maybe Int
  , _inputConfigPgTimeout          :: Maybe Time.NominalDiffTime
  , _inputConfigMapnikInputPlugins :: Maybe FilePath
  , _inputConfigPort               :: Maybe Int
  , _inputConfigTokenCacheSize     :: Maybe Int
  , _inputConfigLayers             :: MapStrict.Map Text Layer.LayerDetails
  , _inputConfigTileBuffer         :: Maybe DGTT.Pixels
  } deriving (Show, Generic)

makeLenses ''InputConfig

instance Aeson.ToJSON InputConfig where
  toJSON ic = Aeson.object $ catMaybes
    [ ("db-connection" Aeson..=)        <$> Just (_inputConfigPgConnection ic)
    , ("db-pool-size" Aeson..=)         <$> _inputConfigPgPoolSize ic
    , ("db-timeout" Aeson..=)           <$> _inputConfigPgTimeout ic
    , ("mapnik-input-plugins" Aeson..=) <$> _inputConfigMapnikInputPlugins ic
    , ("port" Aeson..=)                 <$> _inputConfigPort ic
    , ("token-cache-size" Aeson..=)     <$> _inputConfigTokenCacheSize ic
    , ("layers" Aeson..=)               <$> Just (_inputConfigLayers ic)
    , ("tile-buffer" Aeson..=)          <$> Just (_inputConfigTileBuffer ic)
    ]

instance Aeson.FromJSON InputConfig where
  parseJSON = Aeson.withObject "Config" $ \o -> InputConfig
    <$> o Aeson..:  "db-connection"
    <*> o Aeson..:? "db-pool-size"
    <*> o Aeson..:? "db-timeout"
    <*> o Aeson..:? "mapnik-input-plugins"
    <*> o Aeson..:? "port"
    <*> o Aeson..:? "token-cache-size"
    <*> o Aeson..:  "layers"
    <*> o Aeson..:? "tile-buffer"

emptyInputConfig :: InputConfig
emptyInputConfig = InputConfig "" Nothing Nothing Nothing Nothing Nothing (MapStrict.fromList []) Nothing

data Config = Config
  { _configPgConnection       :: Text
  , _configPgPoolSize         :: Int
  , _configPgTimeout          :: Time.NominalDiffTime
  , _configMapnikInputPlugins :: FilePath
  , _configPort               :: Int
  , _configTokenCacheSize     :: Int
  , _configLayers             :: MapStrict.Map Text Layer.LayerDetails
  , _configTileBuffer         :: DGTT.Pixels
  } deriving (Show, Generic)

makeLenses ''Config

instance Aeson.ToJSON Config where
  toJSON c = Aeson.object
    [ "db-connection"        Aeson..= _configPgConnection c
    , "db-pool-size"         Aeson..= _configPgPoolSize c
    , "db-timeout"           Aeson..= _configPgTimeout c
    , "mapnik-input-plugins" Aeson..= _configMapnikInputPlugins c
    , "port"                 Aeson..= _configPort c
    , "token-cache-size"     Aeson..= _configTokenCacheSize c
    , "layers"               Aeson..= _configLayers c
    , "tile-buffer"          Aeson..= _configTileBuffer c
    ]

newtype CmdLine = CmdLine
  { configFile :: FilePath
  } deriving Generic

instance ParseRecord CmdLine

defaultTileSize :: DGTT.Pixels
defaultTileSize = 2048
