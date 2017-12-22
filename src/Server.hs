{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Server where

import qualified Control.Monad.Trans.Reader as TR
import qualified Network.Wai                as W
import           Servant
import qualified Servant.Utils.Enter        as SE

import           Controllers
import           Routes
import           Types

runServer :: ServerState -> W.Application
runServer s = serve hastileApi (createServer s)

createServer :: ServerState -> Server HastileApi
createServer s = SE.enter (toHandler s) hastileServer

-- Natural Transformation of Types.ActionHandler.ActionHandler to Servant.Handler
toHandler :: ServerState -> ActionHandler :~> Handler
toHandler s = NT (flip TR.runReaderT s . runActionHandler)
