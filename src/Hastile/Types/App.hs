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
import qualified Control.Monad.Reader          as ControlMonadReader
import qualified Data.Geometry.Types.Geography as DataGeometryTypesGeography
import qualified Data.LruCache.IO              as LRU
import qualified Data.Text                     as DataText
import qualified Hasql.Pool                    as HasqlPool
import qualified Servant
import qualified STMContainers.Map             as STMMap

import qualified Hastile.Types.Config          as Config
import qualified Hastile.Types.Layer           as Layer
import qualified Hastile.Types.Token           as Token

data ServerState = ServerState
  { _ssPool                    :: HasqlPool.Pool
  , _ssPluginDir               :: FilePath
  , _ssConfigFile              :: FilePath
  , _ssOriginalConfig          :: Config.Config
  , _ssStateLayers             :: STMMap.Map DataText.Text Layer.Layer
  , _ssTokenAuthorisationCache :: Token.Cache
  }

ControlLens.makeLenses ''ServerState

ssBuffer :: ControlLens.Lens' ServerState DataGeometryTypesGeography.Pixels
ssBuffer = ssOriginalConfig . Config.configTileBuffer

newtype ActionHandler a = ActionHandler
  { runActionHandler :: ControlMonadReader.ReaderT ServerState Servant.Handler a
  } deriving (Functor, Applicative, Monad, ControlMonadReader.MonadReader ServerState, ControlMonadExcept.MonadError Servant.ServantErr, ControlMonadReader.MonadIO)

err204 :: Servant.ServantErr
err204 = Servant.ServantErr
  { errHTTPCode = 204
  , errReasonPhrase = "No Content"
  , errBody = ""
  , errHeaders = []
  }
