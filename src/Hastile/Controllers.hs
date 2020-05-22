{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Controllers
  ( publicHastileServer
  , authenticatedHastileServer
  ) where

import           Control.Monad.Error.Class
import qualified Control.Monad.IO.Class     as MonadIO
import qualified Control.Monad.Reader.Class as MonadReaderClass
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Servant

import qualified Hastile.Controllers.Layer  as Layer
import qualified Hastile.Controllers.Token  as Token
import qualified Hastile.Routes             as Routes
import qualified Hastile.Types.App          as App
import qualified Hastile.Types.Config       as Config

publicHastileServer :: (MonadIO.MonadIO m) => Servant.ServerT Routes.PublicHastileApi (App.ActionHandler m)
publicHastileServer = returnConfiguration
  Servant.:<|> fmap App.RawHtml (liftIO $ BS.readFile "static/index.html")
  Servant.:<|> Layer.layerServerPublic

authenticatedHastileServer :: (MonadIO.MonadIO m) => Servant.ServerT Routes.AuthenticatedHastileApi (App.ActionHandler m)
authenticatedHastileServer = returnConfiguration
  Servant.:<|> Token.tokenServer
  Servant.:<|> Layer.layerServerAuthenticated

returnConfiguration :: (MonadIO.MonadIO m) => (App.ActionHandler m) Config.InputConfig
returnConfiguration = do
  cfgFile <- MonadReaderClass.asks App._ssConfigFile
  configBs <- MonadIO.liftIO $ ByteStringLazyChar8.readFile cfgFile
  case Aeson.eitherDecode configBs of
    Left e  -> throwError $ Servant.err500 { Servant.errBody = ByteStringLazyChar8.pack $ show e }
    Right c -> pure c
