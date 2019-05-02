{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.DB.Table where

import qualified Control.Exception.Base as ControlException
import qualified Data.Either            as DataEither
import qualified Data.Map.Strict        as DataMapStrict
import qualified Data.Text.Encoding     as TextEncoding
import qualified Hasql.Pool             as HasqlPool
import qualified Katip

import qualified Hastile.Lib.Layer      as LibLayer
import qualified Hastile.Lib.Log        as LibLog
import qualified Hastile.Types.Config   as Config
import qualified Hastile.Types.Layer    as Layer

checkConfig :: Katip.LogEnv -> FilePath -> Config.Config -> IO ()
checkConfig logEnv cfgFile Config.Config{..} = do
  pool <- HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection)
  let layers = map (uncurry Layer.Layer) $ DataMapStrict.toList _configLayers
  result <- mapM (LibLayer.checkLayerExists pool) layers
  case DataEither.lefts result of
    [] ->
      pure ()
    errs ->
      ControlException.bracket (pure logEnv) (\_ -> pure ()) $ \le ->
        Katip.runKatipContextT le (mempty :: Katip.LogContexts) mempty (LibLog.logErrors cfgFile errs)
  HasqlPool.release pool

