{-# LANGUAGE OverloadedStrings #-}

module Hastile.Lib.LayerSpec where

import qualified Control.Monad.IO.Class       as IOClass
import qualified Data.ByteString.Char8        as ByteString
import qualified Data.LruCache.IO             as LRUIO
import qualified Hasql.Pool                   as Pool
import           Test.Hspec                   (Spec, before, describe, it,
                                               runIO, shouldBe)

import qualified Hastile.DB.Token             as TokenDB
import qualified Hastile.Lib.Layer            as LayerLib
import qualified Hastile.Lib.Token            as TokenLib
import qualified Hastile.Types.Layer          as Layer
import qualified Hastile.Types.Layer.Security as LayerSecurity
import qualified Hastile.Types.Token          as Token

spec :: Spec
spec = do
  settings <- runIO $ readFile "db/env/test"
  pool <- runIO $ Pool.acquire (1, 10, ByteString.pack settings)
  testCheckLayerAuthorisation pool
  testCheckPrivateLayerAuthorisation pool
  testFetchAuthorisedLayersForToken pool
  runIO $ Pool.release pool

beforeEach :: IOClass.MonadIO m => Pool.Pool -> m Token.Cache
beforeEach pool = do
  cache <- IOClass.liftIO $ LRUIO.newLruHandle 1
  _ <- IOClass.liftIO $ TokenDB.clearTokens pool
  _ <- IOClass.liftIO $ TokenLib.updateOrInsertToken pool cache authorisedToken
  _ <- IOClass.liftIO $ TokenLib.updateOrInsertToken pool cache unauthorisedToken
  pure cache

testCheckLayerAuthorisation :: Pool.Pool -> Spec
testCheckLayerAuthorisation pool =
  before (beforeEach pool) $
    describe "testCheckLayerAuthorisation" $ do
      it "should authorise a public layer when no token is given" $ \cache -> do
        let noToken = Nothing
        layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache publicLayer noToken
        layerAuthorisation `shouldBe` LayerSecurity.Authorised
      it "should authorise a public layer when a token is given" $ \cache -> do
        let token = Just authorisedToken'
        layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache publicLayer token
        layerAuthorisation `shouldBe` LayerSecurity.Authorised
      it "should authorise a private layer if token has access" $ \cache -> do
        let token = Just authorisedToken'
        layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache privateLayer token
        layerAuthorisation `shouldBe` LayerSecurity.Authorised
      it "should not authorise a private layer if token does not have access" $ \cache -> do
        let token = Just unauthorisedToken'
        layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache privateLayer token
        layerAuthorisation `shouldBe` LayerSecurity.Unauthorised
      it "should not authorise a private layer if no token is given" $ \cache -> do
        let noToken = Nothing
        layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache privateLayer noToken
        layerAuthorisation `shouldBe` LayerSecurity.Unauthorised

testCheckPrivateLayerAuthorisation :: Pool.Pool -> Spec
testCheckPrivateLayerAuthorisation pool =
  before (beforeEach pool) $
    describe "testCheckPrivateLayerAuthorisation" $ do
      it "should authorise a private layer if token has access" $ \cache -> do
        let token = Just authorisedToken'
        layerAuthorisation <- LayerLib.checkPrivateLayerAuthorisation pool cache privateLayer token
        layerAuthorisation `shouldBe` LayerSecurity.Authorised
      it "should not authorise a private layer if token does not have access" $ \cache -> do
        let token = Just unauthorisedToken'
        layerAuthorisation <- LayerLib.checkPrivateLayerAuthorisation pool cache privateLayer token
        layerAuthorisation `shouldBe` LayerSecurity.Unauthorised
      it "should not authorise a private layer if no token is given" $ \cache -> do
        let noToken = Nothing
        layerAuthorisation <- LayerLib.checkPrivateLayerAuthorisation pool cache privateLayer noToken
        layerAuthorisation `shouldBe` LayerSecurity.Unauthorised

testFetchAuthorisedLayersForToken :: Pool.Pool -> Spec
testFetchAuthorisedLayersForToken pool =
  before (beforeEach pool) $
    describe "testFetchAuthorisedLayersForToken" $ do
      it "should return authorised layers if token is in db" $ \_ -> do
        layers <- LayerLib.fetchAuthorisedLayersForToken pool authorisedToken'
        layers `shouldBe` authorisedTokenLayers
      it "should return an empty list if token is not in db" $ \_ -> do
        layers <- LayerLib.fetchAuthorisedLayersForToken pool "non-existent-token"
        layers `shouldBe` []

authorisedToken :: Token.TokenAuthorisation
authorisedToken =
  Token.TokenAuthorisation authorisedToken' authorisedTokenLayers

unauthorisedToken :: Token.TokenAuthorisation
unauthorisedToken =
  Token.TokenAuthorisation unauthorisedToken' unauthorisedTokenLayers

authorisedToken' :: Token.Token
authorisedToken' = "abcd"

unauthorisedToken' :: Token.Token
unauthorisedToken' = "defg"

authorisedTokenLayers :: Token.Layers
authorisedTokenLayers = [Layer._layerName privateLayer, "other_layer"]

unauthorisedTokenLayers :: Token.Layers
unauthorisedTokenLayers = ["other_layer"]

publicLayer :: Layer.Layer
publicLayer =
  Layer.Layer
    { Layer._layerName    = "public_layer"
    , Layer._layerSettings =
        Layer.LayerSettings
          { Layer._layerSecurity     = Just LayerSecurity.Public
          , Layer._layerFormat       = Nothing
          , Layer._layerTableName    = Nothing
          , Layer._layerQuantize     = Nothing
          , Layer._layerAlgorithms   = Nothing
          , Layer._layerLastModified = Nothing
          }
    }

privateLayer :: Layer.Layer
privateLayer =
  Layer.Layer
    { Layer._layerName    = "private_layer"
    , Layer._layerSettings =
        Layer.LayerSettings
          { Layer._layerSecurity     = Just LayerSecurity.Private
          , Layer._layerFormat       = Nothing
          , Layer._layerTableName    = Nothing
          , Layer._layerQuantize     = Nothing
          , Layer._layerAlgorithms   = Nothing
          , Layer._layerLastModified = Nothing
          }
    }
