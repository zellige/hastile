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
import           Data.Aeson                    as A
import qualified Data.Geometry.Types.Geography as DGTT
import           Data.Map.Strict               as M
import           Data.Maybe                    (catMaybes)
import qualified Data.Time                     as DT
import           Options.Generic

import qualified Hastile.Types.Layer           as Layer

data InputConfig = InputConfig
  { _inputConfigPgConnection       :: Text
  , _inputConfigPgPoolSize         :: Maybe Int
  , _inputConfigPgTimeout          :: Maybe DT.NominalDiffTime
  , _inputConfigMapnikInputPlugins :: Maybe FilePath
  , _inputConfigPort               :: Maybe Int
  , _inputConfigLayers             :: M.Map Text Layer.LayerDetails
  , _inputConfigTileBuffer         :: Maybe DGTT.Pixels
  } deriving (Show, Generic)

makeLenses ''InputConfig

instance ToJSON InputConfig where
  toJSON ic = object $ catMaybes
    [ ("db-connection" .=)        <$> Just (_inputConfigPgConnection ic)
    , ("db-pool-size" .=)         <$> _inputConfigPgPoolSize ic
    , ("db-timeout" .=)           <$> _inputConfigPgTimeout ic
    , ("mapnik-input-plugins" .=) <$> _inputConfigMapnikInputPlugins ic
    , ("port" .=)                 <$> _inputConfigPort ic
    , ("layers" .=)               <$> Just (_inputConfigLayers ic)
    , ("tile-buffer" .=)          <$> Just (_inputConfigTileBuffer ic)
    ]

instance FromJSON InputConfig where
  parseJSON = withObject "Config" $ \o -> InputConfig
    <$> o .:  "db-connection"
    <*> o .:? "db-pool-size"
    <*> o .:? "db-timeout"
    <*> o .:? "mapnik-input-plugins"
    <*> o .:? "port"
    <*> o .:  "layers"
    <*> o .:? "tile-buffer"

emptyInputConfig :: InputConfig
emptyInputConfig = InputConfig "" Nothing Nothing Nothing Nothing (fromList []) Nothing

data Config = Config
  { _configPgConnection       :: Text
  , _configPgPoolSize         :: Int
  , _configPgTimeout          :: DT.NominalDiffTime
  , _configMapnikInputPlugins :: FilePath
  , _configPort               :: Int
  , _configLayers             :: M.Map Text Layer.LayerDetails
  , _configTileBuffer         :: DGTT.Pixels
  } deriving (Show, Generic)

makeLenses ''Config

instance ToJSON Config where
  toJSON c = object
    [ "db-connection"        .= _configPgConnection c
    , "db-pool-size"         .= _configPgPoolSize c
    , "db-timeout"           .= _configPgTimeout c
    , "mapnik-input-plugins" .= _configMapnikInputPlugins c
    , "port"                 .= _configPort c
    , "layers"               .= _configLayers c
    , "tile-buffer"          .= _configTileBuffer c
    ]

newtype CmdLine = CmdLine
  { configFile :: FilePath
  } deriving Generic

instance ParseRecord CmdLine

defaultTileSize :: DGTT.Pixels
defaultTileSize = 2048
