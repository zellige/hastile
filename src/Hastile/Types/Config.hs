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

module Hastile.Types.Config where

import           Control.Lens                  (makeLenses)
import qualified Control.Monad.IO.Class        as MonadIO
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Encode.Pretty      as AesonPretty
import qualified Data.ByteString.Lazy.Char8    as ByteStringLazyChar8
import qualified Data.Geometry.Types.Geography as GeometryTypesGeography
import qualified Data.Map                      as Map
import qualified Data.Map.Strict               as MapStrict
import qualified Data.Maybe                    as Maybe
import qualified Data.Text                     as Text
import qualified Data.Time                     as Time
import qualified GHC.Conc                      as GhcConc
import qualified ListT
import           Options.Generic               (Generic, ParseRecord)
import qualified STMContainers.Map             as STMMap

import qualified Hastile.Types.Layer           as Layer

defaultTileSize :: GeometryTypesGeography.Pixels
defaultTileSize = 2048

data InputConfig = InputConfig
  { _inputConfigEnvironment    :: Maybe Text.Text
  , _inputConfigAccessLog      :: Maybe Text.Text
  , _inputConfigAppLog         :: Maybe Text.Text
  , _inputConfigPgConnection   :: Text.Text
  , _inputConfigPgPoolSize     :: Maybe Int
  , _inputConfigPgTimeout      :: Maybe Time.NominalDiffTime
  , _inputConfigProtocolHost   :: Maybe Text.Text
  , _inputConfigPort           :: Maybe Int
  , _inputConfigTokenCacheSize :: Maybe Int
  , _inputConfigLayers         :: MapStrict.Map Text.Text Layer.LayerDetails
  , _inputConfigTileBuffer     :: Maybe GeometryTypesGeography.Pixels
  } deriving (Show, Generic)

makeLenses ''InputConfig

instance Aeson.ToJSON InputConfig where
  toJSON ic = Aeson.object $ Maybe.catMaybes
    [ ("environment"  Aeson..=)         <$> Just (_inputConfigPgConnection ic)
    , ("access-log" Aeson..=)           <$> _inputConfigAccessLog ic
    , ("app-log" Aeson..=)              <$> _inputConfigAppLog ic
    , ("db-connection" Aeson..=)        <$> Just (_inputConfigPgConnection ic)
    , ("db-pool-size" Aeson..=)         <$> _inputConfigPgPoolSize ic
    , ("db-timeout" Aeson..=)           <$> _inputConfigPgTimeout ic
    , ("host" Aeson..=)                 <$> _inputConfigProtocolHost ic
    , ("port" Aeson..=)                 <$> _inputConfigPort ic
    , ("token-cache-size" Aeson..=)     <$> _inputConfigTokenCacheSize ic
    , ("layers" Aeson..=)               <$> Just (_inputConfigLayers ic)
    , ("tile-buffer" Aeson..=)          <$> Just (_inputConfigTileBuffer ic)
    ]

instance Aeson.FromJSON InputConfig where
  parseJSON = Aeson.withObject "Config" $ \o -> InputConfig
    <$> o Aeson..:? "environment"
    <*> o Aeson..:? "access-log"
    <*> o Aeson..:? "app-log"
    <*> o Aeson..:  "db-connection"
    <*> o Aeson..:? "db-pool-size"
    <*> o Aeson..:? "db-timeout"
    <*> o Aeson..:? "host"
    <*> o Aeson..:? "port"
    <*> o Aeson..:? "token-cache-size"
    <*> o Aeson..:  "layers"
    <*> o Aeson..:? "tile-buffer"

emptyInputConfig :: InputConfig
emptyInputConfig = InputConfig Nothing Nothing Nothing "" Nothing Nothing Nothing Nothing Nothing (MapStrict.fromList []) Nothing

data Config = Config
  { _configEnvironment    :: Text.Text
  , _configAccessLog      :: Text.Text
  , _configAppLog         :: Text.Text
  , _configPgConnection   :: Text.Text
  , _configPgPoolSize     :: Int
  , _configPgTimeout      :: Time.NominalDiffTime
  , _configProtocolHost   :: Text.Text
  , _configPort           :: Int
  , _configTokenCacheSize :: Int
  , _configLayers         :: MapStrict.Map Text.Text Layer.LayerDetails
  , _configTileBuffer     :: GeometryTypesGeography.Pixels
  } deriving (Show, Generic)

makeLenses ''Config

instance Aeson.ToJSON Config where
  toJSON c = Aeson.object
    [ "environment"          Aeson..= _configEnvironment c
    , "access-log"           Aeson..= _configAccessLog c
    , "app-log"              Aeson..= _configAppLog c
    , "db-connection"        Aeson..= _configPgConnection c
    , "db-pool-size"         Aeson..= _configPgPoolSize c
    , "db-timeout"           Aeson..= _configPgTimeout c
    , "host"                 Aeson..= _configProtocolHost c
    , "port"                 Aeson..= _configPort c
    , "token-cache-size"     Aeson..= _configTokenCacheSize c
    , "layers"               Aeson..= _configLayers c
    , "tile-buffer"          Aeson..= _configTileBuffer c
    ]

newtype CmdLine = CmdLine
  { configFile :: FilePath
  } deriving Generic

instance ParseRecord CmdLine

addLayers :: (MonadIO.MonadIO m) => [Layer.Layer] -> STMMap.Map Text.Text Layer.Layer -> m [(Text.Text, Layer.LayerDetails)]
addLayers layers ls = do
  MonadIO.liftIO . GhcConc.atomically $ mapM_ (\l -> STMMap.insert l (Layer._layerName l) ls) layers
  newLayers <- MonadIO.liftIO . GhcConc.atomically $ stmMapToList ls
  pure $ fmap (\(k, v) -> (k, Layer._layerDetails v)) newLayers

writeLayers :: MonadIO.MonadIO m => [(Text.Text, Layer.LayerDetails)] -> Config -> FilePath -> m ()
writeLayers newLayers originalCfg cfgFile =
  MonadIO.liftIO $ ByteStringLazyChar8.writeFile cfgFile (AesonPretty.encodePretty (originalCfg {_configLayers = Map.fromList newLayers}))

stmMapToList :: STMMap.Map k v -> GhcConc.STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STMMap.stream


