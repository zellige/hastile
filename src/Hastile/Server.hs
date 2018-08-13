{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Server where

import qualified Control.Monad.Trans.Reader as TR
import qualified Network.Wai                as W
import           Servant
import qualified Servant.Server             as SS

import           Hastile.Controllers
import           Hastile.Routes
import           Hastile.Types

runServer :: ServerState -> W.Application
runServer s = serve hastileApi (createServer s)

createServer :: ServerState -> Server HastileApi
createServer s = SS.hoistServer hastileApi (toHandler s) hastileServer

-- Natural Transformation of Types.ActionHandler.ActionHandler to Servant.Handler
toHandler :: ServerState -> ActionHandler a -> Handler a
toHandler s = flip TR.runReaderT s . runActionHandler
