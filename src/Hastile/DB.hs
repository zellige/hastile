{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB where

import           Control.Monad.IO.Class
import qualified Data.Text                  as Text
import qualified Hasql.Pool                 as HasqlPool
import qualified Hasql.Transaction          as HasqlTransaction
import qualified Hasql.Transaction.Sessions as HasqlTransactionSession

runTransaction :: (MonadIO m) => HasqlTransactionSession.Mode -> HasqlPool.Pool -> HasqlTransaction.Transaction b -> m (Either Text.Text b)
runTransaction mode hpool action  = do
  p <- liftIO $ HasqlPool.use hpool session
  case p of
    Left e  -> pure . Left  $ Text.pack (show e)
    Right r -> pure . Right $ r
  where
    session = HasqlTransactionSession.transaction HasqlTransactionSession.ReadCommitted mode action
