{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Token where

import           Control.Monad.IO.Class
import qualified Data.Int                   as Int
import qualified Data.Text                  as Text
import qualified Hasql.Decoders             as HasqlDecoders
import qualified Hasql.Encoders             as HasqlEncoders
import qualified Hasql.Pool                 as Pool
import qualified Hasql.Statement            as HasqlStatement
import qualified Hasql.Transaction          as HasqlTransaction
import qualified Hasql.Transaction.Sessions as HasqlTransactionSession

import qualified Hastile.DB                 as DB
import qualified Hastile.Types.Token        as Token

getTokensQuery :: HasqlStatement.Statement () [Token.TokenAuthorisation]
getTokensQuery =
  HasqlStatement.Statement sql HasqlEncoders.unit (HasqlDecoders.rowList Token.tokenDecoder) False
  where
    sql = "SELECT token, layers FROM tokens;"

getTokens :: MonadIO m => Pool.Pool -> m (Either Text.Text [Token.TokenAuthorisation])
getTokens pool =
    DB.runTransaction HasqlTransactionSession.Read pool action
  where
    action = HasqlTransaction.statement () getTokensQuery

getTokenQuery :: HasqlStatement.Statement Text.Text Token.Layers
getTokenQuery =
  HasqlStatement.Statement sql (HasqlEncoders.param HasqlEncoders.text) (HasqlDecoders.singleRow Token.layersDecoder) False
  where
    sql = "SELECT layers FROM tokens WHERE token LIKE $1;"

getToken :: MonadIO m => Pool.Pool -> Text.Text -> m (Either Text.Text Token.Layers)
getToken pool token =
  DB.runTransaction HasqlTransactionSession.Read pool action
  where
    action = HasqlTransaction.statement token getTokenQuery

updateOrInsertTokenQuery :: HasqlStatement.Statement Token.TokenAuthorisation ()
updateOrInsertTokenQuery =
  HasqlStatement.Statement sql Token.tokenEncoder HasqlDecoders.unit False
  where
    sql = "INSERT INTO tokens (token, layers) VALUES ($1, $2) ON CONFLICT (token) DO UPDATE SET layers = $2;"

updateOrInsertToken :: MonadIO m => Pool.Pool -> Token.TokenAuthorisation -> m (Either Text.Text ())
updateOrInsertToken pool tokenAuthorisation =
  DB.runTransaction HasqlTransactionSession.Write pool action
  where
    action = HasqlTransaction.statement tokenAuthorisation updateOrInsertTokenQuery

deleteTokenQuery :: HasqlStatement.Statement Text.Text Int.Int64
deleteTokenQuery =
  HasqlStatement.Statement sql (HasqlEncoders.param HasqlEncoders.text) HasqlDecoders.rowsAffected False
  where
    sql = "DELETE FROM tokens WHERE token LIKE $1;"

deleteToken :: MonadIO m => Pool.Pool -> Text.Text -> m (Either Text.Text Int.Int64)
deleteToken pool token =
  DB.runTransaction HasqlTransactionSession.Write pool action
  where
    action = HasqlTransaction.statement token deleteTokenQuery

clearTokensQuery :: HasqlStatement.Statement () Int.Int64
clearTokensQuery =
  HasqlStatement.Statement sql HasqlEncoders.unit HasqlDecoders.rowsAffected False
  where
    sql = "DELETE FROM tokens;"

clearTokens :: MonadIO m => Pool.Pool -> m (Either Text.Text Int.Int64)
clearTokens pool =
  DB.runTransaction HasqlTransactionSession.Write pool action
  where
    action = HasqlTransaction.statement () clearTokensQuery
