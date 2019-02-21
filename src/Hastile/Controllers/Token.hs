{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Controllers.Token where

import           Control.Monad.Error.Class
import qualified Control.Monad.IO.Class     as MonadIO
import qualified Control.Monad.Reader.Class as MonadReaderClass
import qualified Data.ByteString.Lazy.Char8 as LazyByteStringChar8
import qualified Data.Text                  as Text
import qualified Servant


import qualified Hastile.DB.Token           as DBToken
import qualified Hastile.Lib.Token          as LibToken
import qualified Hastile.Routes             as Routes
import qualified Hastile.Types.App          as App
import qualified Hastile.Types.Token        as Token

tokenServer :: (MonadIO.MonadIO m) => Servant.ServerT Routes.TokenApi (App.ActionHandler m)
tokenServer = getTokens
  Servant.:<|> getToken
  Servant.:<|> updateOrInsertToken
  Servant.:<|> deleteToken

getTokens :: (MonadIO.MonadIO m) => App.ActionHandler m [Token.TokenAuthorisation]
getTokens = do
  pool <- MonadReaderClass.asks App._ssPool
  er <- DBToken.getTokens pool
  case er of
    Left err     -> defaultErrorHandler err
    Right tokens -> return tokens

getToken :: (MonadIO.MonadIO m) => Text.Text -> App.ActionHandler m Token.Layers
getToken token = do
  pool <- MonadReaderClass.asks App._ssPool
  er <- DBToken.getToken pool token
  case er of
    Left err     -> defaultErrorHandler err
    Right layers -> return layers

updateOrInsertToken :: (MonadIO.MonadIO m) => Token.TokenAuthorisation -> App.ActionHandler m Text.Text
updateOrInsertToken tokenAuthorisation = do
  pool <- MonadReaderClass.asks App._ssPool
  cache <- MonadReaderClass.asks App._ssTokenAuthorisationCache
  er <- LibToken.updateOrInsertToken pool cache tokenAuthorisation
  case er of
    Left err     -> defaultErrorHandler err
    Right result -> return result

deleteToken :: (MonadIO.MonadIO m) => Text.Text -> App.ActionHandler m Text.Text
deleteToken token = do
  pool <- MonadReaderClass.asks App._ssPool
  cache <- MonadReaderClass.asks App._ssTokenAuthorisationCache
  er <- LibToken.deleteToken pool cache token
  case er of
    Left err     -> defaultErrorHandler err
    Right result -> return result

defaultErrorHandler :: (MonadIO.MonadIO m) => Text.Text -> App.ActionHandler m a
defaultErrorHandler e =  throwError $ Servant.err500 { Servant.errBody = LazyByteStringChar8.pack $ Text.unpack e }
