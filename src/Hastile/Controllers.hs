{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Controllers where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader.Class as MonadReaderClass
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Servant

import qualified Hastile.Controllers.Layer  as Layer
import qualified Hastile.Controllers.Token  as Token
import qualified Hastile.Routes             as Routes
import qualified Hastile.Types.App          as App
import qualified Hastile.Types.Config       as Config

hastileServer :: Servant.ServerT Routes.HastileApi App.ActionHandler
hastileServer = returnConfiguration
  Servant.:<|> Token.tokenServer
  Servant.:<|> Layer.createNewLayer
  Servant.:<|> Layer.layerServer

returnConfiguration :: App.ActionHandler Config.InputConfig
returnConfiguration = do
  cfgFile <- MonadReaderClass.asks App._ssConfigFile
  configBs <- liftIO $ ByteStringLazyChar8.readFile cfgFile
  case Aeson.eitherDecode configBs of
    Left e  -> throwError $ Servant.err500 { Servant.errBody = ByteStringLazyChar8.pack $ show e }
    Right c -> pure c
