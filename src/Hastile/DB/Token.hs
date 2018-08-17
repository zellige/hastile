{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Token where

import           Control.Monad.IO.Class
import qualified Data.Int               as Int
import qualified Data.Text              as Text
import qualified Hasql.Decoders         as HD
import qualified Hasql.Encoders         as HE
import qualified Hasql.Pool             as P
import qualified Hasql.Query            as HQ
import qualified Hasql.Session          as HS
import qualified Hastile.DB             as DB

import qualified Hastile.Types.Token    as Token


getTokensQuery :: HQ.Query () [Token.TokenLayers]
getTokensQuery =
    HQ.statement sql HE.unit (HD.rowsList Token.tokenDecoder) False
  where
    sql = "SELECT token, layers FROM tokens;"

getTokens :: MonadIO m => String -> P.Pool -> m (Either Text.Text [Token.TokenLayers])
getTokens schemaName pool =
    DB.runDBeither pool action
  where
    action =
      DB.schemaSession schemaName >>
      HS.query () getTokensQuery

getTokenQuery :: HQ.Query Text.Text Token.TokenLayers
getTokenQuery =
    HQ.statement sql (HE.value HE.text) (HD.singleRow Token.tokenDecoder) False
  where
    sql = "SELECT token, layers FROM tokens WHERE token LIKE $1;"

getToken :: MonadIO m => String -> P.Pool -> Text.Text -> m (Either Text.Text Token.TokenLayers)
getToken schemaName pool token =
  DB.runDBeither pool action
  where
    action =
      DB.schemaSession schemaName >>
      HS.query token getTokenQuery

updateOrInsertTokenQuery :: HQ.Query Token.TokenLayers ()
updateOrInsertTokenQuery =
    HQ.statement sql Token.tokenEncoder HD.unit False
  where
    sql = "INSERT INTO tokens (token, layers) VALUES ($1, $2) ON CONFLICT (token) DO UPDATE SET layers = $2;"

updateOrInsertToken :: MonadIO m => String -> P.Pool -> Token.TokenLayers -> m (Either Text.Text ())
updateOrInsertToken schemaName pool tokenLayers =
  DB.runDBeither pool action
  where
    action =
      DB.schemaSession schemaName >>
      HS.query tokenLayers updateOrInsertTokenQuery

deleteTokenQuery :: HQ.Query Text.Text Int.Int64
deleteTokenQuery =
    HQ.statement sql (HE.value HE.text) HD.rowsAffected False
  where
    sql = "DELETE FROM tokens WHERE token LIKE $1;"

deleteToken :: MonadIO m => String -> P.Pool -> Text.Text -> m (Either Text.Text Int.Int64)
deleteToken schemaName pool token =
  DB.runDBeither pool action
  where
    action =
      DB.schemaSession schemaName >>
      HS.query token deleteTokenQuery
