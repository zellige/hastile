{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Token where

import           Control.Monad.IO.Class
import qualified Data.Int                   as Int
import qualified Data.Text                  as Text
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as Pool
import qualified Hasql.Query                as Query
import qualified Hasql.Transaction          as Transaction
import qualified Hasql.Transaction.Sessions as Transaction

import qualified Hastile.Types.Token        as Token

getTokensQuery :: Query.Query () [Token.TokenAuthorisation]
getTokensQuery =
    Query.statement sql HE.unit (HD.rowsList Token.tokenDecoder) False
  where
    sql = "SELECT token, layers FROM tokens;"

getTokens :: MonadIO m => Pool.Pool -> m (Either Text.Text [Token.TokenAuthorisation])
getTokens pool =
    runReadTransaction pool action
  where
    action = Transaction.query () getTokensQuery

getTokenQuery :: Query.Query Text.Text Token.Layers
getTokenQuery =
    Query.statement sql (HE.value HE.text) (HD.singleRow Token.layersDecoder) False
  where
    sql = "SELECT layers FROM tokens WHERE token LIKE $1;"

getToken :: MonadIO m => Pool.Pool -> Text.Text -> m (Either Text.Text Token.Layers)
getToken pool token =
  runReadTransaction pool action
  where
    action = Transaction.query token getTokenQuery

updateOrInsertTokenQuery :: Query.Query Token.TokenAuthorisation ()
updateOrInsertTokenQuery =
    Query.statement sql Token.tokenEncoder HD.unit False
  where
    sql = "INSERT INTO tokens (token, layers) VALUES ($1, $2) ON CONFLICT (token) DO UPDATE SET layers = $2;"

updateOrInsertToken :: MonadIO m => Pool.Pool -> Token.TokenAuthorisation -> m (Either Text.Text ())
updateOrInsertToken pool tokenAuthorisation =
  runWriteTransaction pool action
  where
    action = Transaction.query tokenAuthorisation updateOrInsertTokenQuery

deleteTokenQuery :: Query.Query Text.Text Int.Int64
deleteTokenQuery =
    Query.statement sql (HE.value HE.text) HD.rowsAffected False
  where
    sql = "DELETE FROM tokens WHERE token LIKE $1;"

deleteToken :: MonadIO m => Pool.Pool -> Text.Text -> m (Either Text.Text Int.Int64)
deleteToken pool token =
  runWriteTransaction pool action
  where
    action = Transaction.query token deleteTokenQuery

clearTokensQuery :: Query.Query () Int.Int64
clearTokensQuery =
    Query.statement sql HE.unit HD.rowsAffected False
  where
    sql = "DELETE FROM tokens;"

clearTokens :: MonadIO m => Pool.Pool -> m (Either Text.Text Int.Int64)
clearTokens pool =
  runWriteTransaction pool action
  where
    action = Transaction.query () clearTokensQuery

runReadTransaction :: (MonadIO m) => Pool.Pool -> Transaction.Transaction b -> m (Either Text.Text b)
runReadTransaction =
  runTransaction Transaction.Read

runWriteTransaction :: (MonadIO m) => Pool.Pool -> Transaction.Transaction b -> m (Either Text.Text b)
runWriteTransaction =
  runTransaction Transaction.Write

runTransaction :: (MonadIO m) => Transaction.Mode -> Pool.Pool -> Transaction.Transaction b -> m (Either Text.Text b)
runTransaction mode hpool action  = do
  p <- liftIO $ Pool.use hpool session
  case p of
    Left e  -> pure . Left  $ Text.pack (show e)
    Right r -> pure . Right $ r
  where
    session = Transaction.transaction
                Transaction.ReadCommitted mode action
