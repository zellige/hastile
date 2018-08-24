{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Lib.Token where

import qualified Control.Monad.IO.Class as IOClass
import qualified Data.IORef             as IORef (atomicModifyIORef')
import qualified Data.LruCache          as LRU
import qualified Data.LruCache.IO       as LRUIO
import qualified Data.Text              as Text
import qualified Hasql.Pool             as Pool

import qualified Hastile.DB.Token       as DB
import qualified Hastile.Types.Token    as Token

updateOrInsertToken :: IOClass.MonadIO m => Pool.Pool -> Token.Cache -> Token.TokenAuthorisation -> m (Either Text.Text Text.Text)
updateOrInsertToken pool cache tokenAuthorisation = do
  er <- DB.updateOrInsertToken pool tokenAuthorisation
  case er of
    Left e   -> pure $ Left e
    Right () -> do
      IOClass.liftIO $ updateCache cache tokenAuthorisation
      pure $ Right "OK"

deleteToken :: IOClass.MonadIO m => Pool.Pool -> Token.Cache -> Text.Text -> m (Either Text.Text Text.Text)
deleteToken pool cache token = do
  er <- DB.deleteToken pool token
  case er of
    Left e                    -> pure $ Left e
    Right numberOfRowsDeleted ->
      case numberOfRowsDeleted of
        1 -> do
          IOClass.liftIO $ updateCache cache (Token.unauthorisedToken token)
          pure $ Right "OK"
        _ -> pure $ Left "Delete failed"

updateCache :: Token.Cache -> Token.TokenAuthorisation -> IO ()
updateCache (LRUIO.LruHandle ref) tokenAuthorisation = do
  IORef.atomicModifyIORef' ref $ \c -> (LRU.insert k v c, ())
  return ()
  where k = Token._token tokenAuthorisation
        v = Token._layers tokenAuthorisation
