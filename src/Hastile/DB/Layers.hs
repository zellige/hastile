{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Layers where

import           Control.Monad.IO.Class
import qualified Data.Int                   as Int
import qualified Data.Text                  as Text
import qualified Hasql.Decoders             as HasqlDecoders
import qualified Hasql.Encoders             as HasqlEncoders
import qualified Hasql.Pool                 as Pool
import qualified Hasql.Statement            as HasqlStatement
import qualified Hasql.Transaction          as HasqlTransaction
import qualified Hasql.Transaction.Sessions as HasqlTransactionSession

import qualified Hastile.Types.Layer        as Layer

getLayersQuery :: HasqlStatement.Statement () [Layer.Layer]
getLayersQuery =
  HasqlStatement.Statement sql HasqlEncoders.unit (HasqlDecoders.rowList Layer.) False
  where
    sql = "SELECT token, layers FROM tokens;"

getLayers :: MonadIO m => Pool.Pool -> m (Either Text.Text [Layer.Layer])
getLayers pool =
    runReadTransaction pool action
  where
    action = HasqlTransaction.statement () getTokensQuery

runReadTransaction :: (MonadIO m) => Pool.Pool -> HasqlTransaction.Transaction b -> m (Either Text.Text b)
runReadTransaction =
  runTransaction HasqlTransactionSession.Read

runTransaction :: (MonadIO m) => HasqlTransactionSession.Mode -> Pool.Pool -> HasqlTransaction.Transaction b -> m (Either Text.Text b)
runTransaction mode hpool action  = do
  p <- liftIO $ Pool.use hpool session
  case p of
    Left e  -> pure . Left  $ Text.pack (show e)
    Right r -> pure . Right $ r
  where
    session = HasqlTransactionSession.transaction HasqlTransactionSession.ReadCommitted mode action

