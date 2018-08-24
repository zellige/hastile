{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Lib.Layer where

import qualified Control.Monad.IO.Class as IOClass
import qualified Data.LruCache.IO       as LRU
import qualified Data.Text              as Text
import qualified Hasql.Pool             as Pool

import qualified Hastile.DB.Token       as DBToken
import qualified Hastile.Types.Layer    as Layer
import qualified Hastile.Types.Token    as Token

checkLayerAuthorisation :: IOClass.MonadIO m => Pool.Pool -> Token.Cache -> Layer.Layer -> Maybe Text.Text -> m Layer.LayerAuthorisation
checkLayerAuthorisation pool cache layer maybeToken =
  case Layer._layerSecurity layer of
    Layer.LayerSecurityPublic ->
      pure Layer.Authorised
    Layer.LayerSecurityPrivate ->
      checkPrivateLayerAuthorisation pool cache layer maybeToken

checkPrivateLayerAuthorisation :: IOClass.MonadIO m => Pool.Pool -> Token.Cache -> Layer.Layer -> Maybe Text.Text -> m Layer.LayerAuthorisation
checkPrivateLayerAuthorisation pool cache layer maybeToken =
  case maybeToken of
    Just token -> do
      foundLayers <- IOClass.liftIO $ LRU.cached cache token (fetchAuthorisedLayersForToken pool token)
      if Layer._layerName layer `elem` foundLayers
        then pure Layer.Authorised
        else pure Layer.Unauthorised
    Nothing ->
      pure Layer.Unauthorised

fetchAuthorisedLayersForToken :: IOClass.MonadIO m => Pool.Pool -> Text.Text -> m [Token.Layer]
fetchAuthorisedLayersForToken pool token = do
  er <- DBToken.getToken pool token
  case er of
    Left _            -> pure []
    Right foundLayers -> pure foundLayers
