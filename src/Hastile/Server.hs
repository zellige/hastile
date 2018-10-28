{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Server where

import qualified Control.Monad.Trans.Reader as MonadTransReader
import qualified Network.Wai                as Wai
import           Servant
import qualified Servant.Server             as ServantServer

import           Hastile.Controllers
import           Hastile.Routes
import           Hastile.Types.App          as App

runServer :: App.ServerState -> Wai.Application
runServer s = serve hastileApi (createServer s)

createServer :: App.ServerState -> Server HastileApi
createServer s = ServantServer.hoistServer hastileApi (toHandler s) hastileServer

-- Natural Transformation of Types.ActionHandler.ActionHandler to Servant.Handler
toHandler :: ServerState -> App.ActionHandler a -> Handler a
toHandler s = flip MonadTransReader.runReaderT s . runActionHandler
