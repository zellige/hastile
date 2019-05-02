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

module Hastile.Types.App where

import qualified Control.Lens                  as ControlLens
import qualified Control.Monad.Except          as ControlMonadExcept
import qualified Control.Monad.IO.Class        as MonadIO
import qualified Control.Monad.Logger          as MonadLogger
import qualified Control.Monad.Reader          as ControlMonadReader
import qualified Data.Geometry.Types.Geography as DataGeometryTypesGeography
import qualified Data.Text                     as Text
import qualified Data.Time                     as Time
import qualified Hasql.Pool                    as HasqlPool
import qualified Katip
import qualified Prometheus
import qualified Servant
import qualified STMContainers.Map             as STMMap

import qualified Hastile.Types.Config          as Config
import qualified Hastile.Types.Layer           as Layer
import qualified Hastile.Types.Logger          as Logger
import qualified Hastile.Types.Token           as Token

data ServerState = ServerState
  { _ssPool                    :: HasqlPool.Pool
  , _ssConfigFile              :: FilePath
  , _ssOriginalConfig          :: Config.Config
  , _ssStateLayers             :: STMMap.Map Text.Text Layer.Layer
  , _ssTokenAuthorisationCache :: Token.Cache
  , _ssLogEnv                  :: Katip.LogEnv
  , _ssLayerMetric             :: Prometheus.Vector (Text.Text, Text.Text) Prometheus.Counter
  , _ssStartTime               :: Time.UTCTime
  }

ControlLens.makeLenses ''ServerState

ssBuffer :: ControlLens.Lens' ServerState DataGeometryTypesGeography.Pixels
ssBuffer = ssOriginalConfig . Config.configTileBuffer

newtype ActionHandler m a = ActionHandler
  { runActionHandler :: ControlMonadReader.ReaderT ServerState (ControlMonadExcept.ExceptT Servant.ServantErr m) a
  } deriving (Functor, Applicative, Monad, ControlMonadReader.MonadReader ServerState, ControlMonadExcept.MonadError Servant.ServantErr, ControlMonadReader.MonadIO)

instance (MonadIO.MonadIO m) => Katip.Katip (ActionHandler m) where
  getLogEnv = ControlMonadReader.asks _ssLogEnv
  localLogEnv = error "not implemented"

instance (MonadIO.MonadIO m) => MonadLogger.MonadLogger (ActionHandler m) where
  monadLoggerLog = Logger.adapt Katip.logMsg

instance (MonadIO.MonadIO m) => MonadLogger.MonadLogger (Katip.KatipT m) where
  monadLoggerLog = Logger.adapt Katip.logMsg

err204 :: Servant.ServantErr
err204 = Servant.ServantErr
  { errHTTPCode = 204
  , errReasonPhrase = "No Content"
  , errBody = ""
  , errHeaders = []
  }
