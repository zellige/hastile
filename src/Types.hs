{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Types where

import           Data.Aeson
import           Data.Map        as M
import           Data.Text       as T
import           Data.Time
import           Hasql.Pool      as P
import           Options.Generic

data CmdLine = CmdLine { _cmdLineConfigFile :: FilePath
                       } deriving Generic
instance ParseRecord CmdLine

data Layer = Layer { _layerQuery        :: Text
                   , _layerLastModified :: UTCTime
                   } deriving (Show, Eq, Generic)

instance FromJSON Layer where

data Config = Config { _configPgConnection       :: Text
                     , _configMapnikInputPlugins :: Maybe FilePath
                     , _configPort               :: Maybe Int
                     , _configPgPoolSize         :: Maybe Int
                     , _configPgTimeout          :: Maybe NominalDiffTime
                     , _configLayers             :: Map Text Layer
                     } deriving (Show, Generic)

instance FromJSON Config where

-- TODO: make lenses!
data ServerState = ServerState { _pool        :: P.Pool
                               , _pluginDir   :: FilePath
                               , _startTime   :: String
                               , _stateLayers :: Map Text Layer
                               }

data TileFeature = TileFeature { _geometry   :: Value
                               , _properties :: Map Text Text
                               }
