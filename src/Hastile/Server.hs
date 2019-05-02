{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Server ( runServer ) where

import qualified Control.Monad.Trans.Reader as MonadTransReader
import qualified Network.Wai                as Wai
import qualified Servant
import qualified Servant.Server             as ServantServer

import qualified Hastile.Controllers        as Controllers
import qualified Hastile.Routes             as Routes
import qualified Hastile.Types.App          as App

runServer :: App.AppMode -> App.ServerState -> Wai.Application
runServer appMode serverState =
  case appMode of
    App.Public ->
      Servant.serve Routes.publicHastileApi (createPublicServer serverState)
    App.Authenticated ->
      Servant.serve Routes.authenticatedHastileApi (createAuthenticatedServer serverState)

createPublicServer :: App.ServerState -> Servant.Server Routes.PublicHastileApi
createPublicServer serverState = ServantServer.hoistServer Routes.publicHastileApi (toHandler serverState) Controllers.publicHastileServer

createAuthenticatedServer :: App.ServerState -> Servant.Server Routes.AuthenticatedHastileApi
createAuthenticatedServer serverState = ServantServer.hoistServer Routes.authenticatedHastileApi (toHandler serverState) Controllers.authenticatedHastileServer

-- Natural Transformation of Types.ActionHandler.ActionHandler to Servant.Handler
toHandler :: App.ServerState -> App.ActionHandler IO a -> Servant.Handler a
toHandler serverState app = Servant.Handler $ MonadTransReader.runReaderT (App.runActionHandler app) serverState
