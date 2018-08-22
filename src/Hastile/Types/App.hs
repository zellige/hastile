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

import           Control.Lens                  (Lens', makeLenses)
import           Control.Monad.Except          (MonadError)
import           Control.Monad.Reader          (MonadIO, MonadReader, ReaderT)
import qualified Data.Geometry.Types.Geography as DGTT
import qualified Data.LruCache.IO              as LRU
import           Hasql.Pool                    as P
import           Options.Generic
import           Servant
import           STMContainers.Map             as STM

import qualified Hastile.Types.Config          as Config
import qualified Hastile.Types.Layer           as Layer
import qualified Hastile.Types.Token           as Token

data ServerState = ServerState
  { _ssPool                    :: P.Pool
  , _ssPluginDir               :: FilePath
  , _ssConfigFile              :: FilePath
  , _ssOriginalConfig          :: Config.Config
  , _ssStateLayers             :: STM.Map Text Layer.Layer
  , _ssTokenAuthorisationCache :: LRU.LruHandle Token.Token Token.Layers
  }

makeLenses ''ServerState

ssBuffer :: Lens' ServerState DGTT.Pixels
ssBuffer = ssOriginalConfig . Config.configTileBuffer

newtype ActionHandler a = ActionHandler
  { runActionHandler :: ReaderT ServerState Handler a
  } deriving (Functor, Applicative, Monad, MonadReader ServerState, MonadError ServantErr, MonadIO)

err204 :: ServantErr
err204 = ServantErr { errHTTPCode = 204
                    , errReasonPhrase = "No Content"
                    , errBody = ""
                    , errHeaders = []
                    }
