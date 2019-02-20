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
import qualified Data.Geometry.Types.Geography as GeometryTypesGeography
import qualified Data.Map.Strict               as MapStrict
import qualified Data.Maybe                    as Maybe
import qualified Data.Time                     as Time
import           Options.Generic

import qualified Hastile.Types.Layer           as Layer

data InputConfig = InputConfig
  { _inputConfigEnvironment    :: Maybe Text
  , _inputConfigPgConnection   :: Text
  , _inputConfigPgPoolSize     :: Maybe Int
  , _inputConfigPgTimeout      :: Maybe Time.NominalDiffTime
  , _inputConfigPort           :: Maybe Int
  , _inputConfigTokenCacheSize :: Maybe Int
  , _inputConfigLayers         :: MapStrict.Map Text Layer.LayerDetails
  , _inputConfigTileBuffer     :: Maybe GeometryTypesGeography.Pixels
  } deriving (Show, Generic)

makeLenses ''InputConfig

instance Aeson.ToJSON InputConfig where
  toJSON ic = Aeson.object $ Maybe.catMaybes
    [ ("environment"  Aeson..=)         <$> Just (_inputConfigPgConnection ic)
    , ("db-connection" Aeson..=)        <$> Just (_inputConfigPgConnection ic)
    , ("db-pool-size" Aeson..=)         <$> _inputConfigPgPoolSize ic
    , ("db-timeout" Aeson..=)           <$> _inputConfigPgTimeout ic
    , ("port" Aeson..=)                 <$> _inputConfigPort ic
    , ("token-cache-size" Aeson..=)     <$> _inputConfigTokenCacheSize ic
    , ("layers" Aeson..=)               <$> Just (_inputConfigLayers ic)
    , ("tile-buffer" Aeson..=)          <$> Just (_inputConfigTileBuffer ic)
    ]

instance Aeson.FromJSON InputConfig where
  parseJSON = Aeson.withObject "Config" $ \o -> InputConfig
    <$> o Aeson..:? "environment"
    <*> o Aeson..:  "db-connection"
    <*> o Aeson..:? "db-pool-size"
    <*> o Aeson..:? "db-timeout"
    <*> o Aeson..:? "port"
    <*> o Aeson..:? "token-cache-size"
    <*> o Aeson..:  "layers"
    <*> o Aeson..:? "tile-buffer"

emptyInputConfig :: InputConfig
emptyInputConfig = InputConfig Nothing "" Nothing Nothing Nothing Nothing (MapStrict.fromList []) Nothing

data Config = Config
  { _configEnvironment    :: Text
  , _configPgConnection   :: Text
  , _configPgPoolSize     :: Int
  , _configPgTimeout      :: Time.NominalDiffTime
  , _configPort           :: Int
  , _configTokenCacheSize :: Int
  , _configLayers         :: MapStrict.Map Text Layer.LayerDetails
  , _configTileBuffer     :: GeometryTypesGeography.Pixels
  } deriving (Show, Generic)

makeLenses ''Config

instance Aeson.ToJSON Config where
  toJSON c = Aeson.object
    [ "environment"          Aeson..= _configEnvironment c
    , "db-connection"        Aeson..= _configPgConnection c
    , "db-pool-size"         Aeson..= _configPgPoolSize c
    , "db-timeout"           Aeson..= _configPgTimeout c
    , "port"                 Aeson..= _configPort c
    , "token-cache-size"     Aeson..= _configTokenCacheSize c
    , "layers"               Aeson..= _configLayers c
    , "tile-buffer"          Aeson..= _configTileBuffer c
    ]

newtype CmdLine = CmdLine
  { configFile :: FilePath
  } deriving Generic

instance ParseRecord CmdLine

defaultTileSize :: GeometryTypesGeography.Pixels
defaultTileSize = 2048
