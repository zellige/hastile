{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Controllers.Token where

import           Control.Monad.Error.Class
import qualified Control.Monad.Reader.Class as RC
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text                  as T
import qualified Servant                    as S

import qualified Hastile.DB.Token           as DB
import qualified Hastile.Lib.Token          as TokenLib
import qualified Hastile.Routes             as Routes
import qualified Hastile.Types.App          as App
import qualified Hastile.Types.Token        as Token

tokenServer :: S.ServerT Routes.TokenApi App.ActionHandler
tokenServer = getTokens
  S.:<|> getToken
  S.:<|> updateOrInsertToken
  S.:<|> deleteToken

getTokens :: App.ActionHandler [Token.TokenAuthorisation]
getTokens = do
  pool <- RC.asks App._ssPool
  er <- DB.getTokens "public" pool
  case er of
    Left err     -> defaultErrorHandler err
    Right tokens -> return tokens

getToken :: T.Text -> App.ActionHandler Token.Layers
getToken token = do
  pool <- RC.asks App._ssPool
  er <- DB.getToken "public" pool token
  case er of
    Left err     -> defaultErrorHandler err
    Right layers -> return layers

updateOrInsertToken :: Token.TokenAuthorisation -> App.ActionHandler T.Text
updateOrInsertToken tokenAuthorisation = do
  pool <- RC.asks App._ssPool
  cache <- RC.asks App._ssTokenAuthorisationCache
  er <- TokenLib.updateOrInsertToken pool cache tokenAuthorisation
  case er of
    Left err     -> defaultErrorHandler err
    Right result -> return result

deleteToken :: T.Text -> App.ActionHandler T.Text
deleteToken token = do
  pool <- RC.asks App._ssPool
  cache <- RC.asks App._ssTokenAuthorisationCache
  er <- TokenLib.deleteToken pool cache token
  case er of
    Left err     -> defaultErrorHandler err
    Right result -> return result

defaultErrorHandler :: MonadError S.ServantErr m => T.Text -> m a
defaultErrorHandler e =  throwError $ S.err500 { S.errBody = LBS8.pack $ T.unpack e }
