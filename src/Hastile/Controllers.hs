{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Controllers where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader.Class as RC
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Servant                    as S

import qualified Hastile.Controllers.Layer  as Layer
import qualified Hastile.Controllers.Token  as Token
import qualified Hastile.Routes             as Routes
import qualified Hastile.Types.App          as App
import qualified Hastile.Types.Config       as Config

hastileServer :: S.ServerT Routes.HastileApi App.ActionHandler
hastileServer = returnConfiguration
  S.:<|> Token.tokenServer
  S.:<|> Layer.createNewLayer
  S.:<|> Layer.layerServer

returnConfiguration :: App.ActionHandler Config.InputConfig
returnConfiguration = do
  cfgFile <- RC.asks App._ssConfigFile
  configBs <- liftIO $ LBS8.readFile cfgFile
  case A.eitherDecode configBs of
    Left e  -> throwError $ S.err500 { S.errBody = LBS8.pack $ show e }
    Right c -> pure c
