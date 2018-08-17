{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Token where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as BSChar8
import qualified Data.Int               as Int
import           Data.Monoid
import qualified Data.Text              as Text
import qualified Hasql.Decoders         as HD
import qualified Hasql.Encoders         as HE
import qualified Hasql.Pool             as P
import qualified Hasql.Query            as HQ
import qualified Hasql.Session          as HS

import qualified Hastile.Types.Token    as Token

getTokensQuery :: HQ.Query () [Token.TokenLayers]
getTokensQuery =
    HQ.statement sql HE.unit (HD.rowsList Token.tokenDecoder) False
  where
    sql = "SELECT token, layers FROM tokens;"

getTokens :: MonadIO m => String -> P.Pool -> m (Either Text.Text [Token.TokenLayers])
getTokens schemaName pool =
    runDBeither pool action
  where
    action =
      schemaSession schemaName >>
      HS.query () getTokensQuery

getTokenQuery :: HQ.Query Text.Text Token.TokenLayers
getTokenQuery =
    HQ.statement sql (HE.value HE.text) (HD.singleRow Token.tokenDecoder) False
  where
    sql = "SELECT token, layers FROM tokens WHERE token LIKE $1;"

getToken :: MonadIO m => String -> P.Pool -> Text.Text -> m (Either Text.Text Token.TokenLayers)
getToken schemaName pool token =
  runDBeither pool action
  where
    action =
      schemaSession schemaName >>
      HS.query token getTokenQuery

updateOrInsertTokenQuery :: HQ.Query Token.TokenLayers ()
updateOrInsertTokenQuery =
    HQ.statement sql Token.tokenEncoder HD.unit False
  where
    sql = "INSERT INTO tokens (token, layers) VALUES ($1, $2) ON CONFLICT (token) DO UPDATE SET layers = $2;"

updateOrInsertToken :: MonadIO m => String -> P.Pool -> Token.TokenLayers -> m (Either Text.Text ())
updateOrInsertToken schemaName pool tokenLayers =
  runDBeither pool action
  where
    action =
      schemaSession schemaName >>
      HS.query tokenLayers updateOrInsertTokenQuery

deleteTokenQuery :: HQ.Query Text.Text Int.Int64
deleteTokenQuery =
    HQ.statement sql (HE.value HE.text) HD.rowsAffected False
  where
    sql = "DELETE FROM tokens WHERE token LIKE $1;"

deleteToken :: MonadIO m => String -> P.Pool -> Text.Text -> m (Either Text.Text Int.Int64)
deleteToken schemaName pool token =
  runDBeither pool action
  where
    action =
      schemaSession schemaName >>
      HS.query token deleteTokenQuery


runDBeither :: (MonadIO m) => P.Pool -> HS.Session b -> m (Either Text.Text b)
runDBeither hpool action = do
  p <- liftIO $ P.use hpool action
  case p of
    Left e  -> pure . Left  $ Text.pack (show e)
    Right r -> pure . Right $ r

mkSession :: a -> HQ.Query a b -> HS.Session b
mkSession = HS.query

emptySession :: BSChar8.ByteString -> HS.Session ()
emptySession sql = mkSession () $ HQ.statement sql HE.unit HD.unit False

withSchema :: String -> BSChar8.ByteString
withSchema schemaName = "SET search_path TO " <> BSChar8.pack schemaName <> ";"

schemaSession :: String -> HS.Session ()
schemaSession schemaName = emptySession (withSchema schemaName)
