{-# LANGUAGE OverloadedStrings #-}

module Hastile.Lib.LayerSpec where

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.LruCache.IO      as LRUIO
import qualified Data.Map.Strict       as Map
import qualified Data.Time             as Time
import qualified Hasql.Pool            as Pool
import           Test.Hspec            (Spec, describe, it, runIO, shouldBe)

import qualified Hastile.Lib.Layer     as LayerLib
import qualified Hastile.Lib.Token     as TokenLib
import qualified Hastile.Types.Layer   as Layer
import qualified Hastile.Types.Token   as Token

spec :: Spec
spec = do
  settings <- runIO $ readFile "db/env/test"
  pool <- runIO $ Pool.acquire (1, 10, ByteString.pack settings)
  testCheckLayerAuthorisation pool
  testCheckPrivateLayerAuthorisation pool
  testFetchAuthorisedLayersForToken pool
  runIO $ Pool.release pool

testCheckLayerAuthorisation :: Pool.Pool -> Spec
testCheckLayerAuthorisation pool =
  describe "testCheckLayerAuthorisation" $ do
    cache <- runIO $ LRUIO.newLruHandle 1
    _ <- runIO $ setupTokens pool cache
    it "should authorise a public layer when no token is given" $ do
      let noToken = Nothing
      layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache publicLayer noToken
      layerAuthorisation `shouldBe` Layer.Authorised
    it "should authorise a public layer when a token is given" $ do
      let token = Just authorisedToken'
      layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache publicLayer token
      layerAuthorisation `shouldBe` Layer.Authorised
    it "should authorise a private layer if token has access" $ do
      let token = Just authorisedToken'
      layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache privateLayer token
      layerAuthorisation `shouldBe` Layer.Authorised
    it "should not authorise a private layer if token does not have access" $ do
      let token = Just unauthorisedToken'
      layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache privateLayer token
      layerAuthorisation `shouldBe` Layer.Unauthorised
    it "should not authorise a private layer if no token is given" $ do
      let noToken = Nothing
      layerAuthorisation <- LayerLib.checkLayerAuthorisation pool cache privateLayer noToken
      layerAuthorisation `shouldBe` Layer.Unauthorised

testCheckPrivateLayerAuthorisation :: Pool.Pool -> Spec
testCheckPrivateLayerAuthorisation pool =
  describe "testCheckPrivateLayerAuthorisation" $ do
    cache <- runIO $ LRUIO.newLruHandle 1
    _ <- runIO $ setupTokens pool cache
    it "should authorise a private layer if token has access" $ do
      let token = Just authorisedToken'
      layerAuthorisation <- LayerLib.checkPrivateLayerAuthorisation pool cache privateLayer token
      layerAuthorisation `shouldBe` Layer.Authorised
    it "should not authorise a private layer if token does not have access" $ do
      let token = Just unauthorisedToken'
      layerAuthorisation <- LayerLib.checkPrivateLayerAuthorisation pool cache privateLayer token
      layerAuthorisation `shouldBe` Layer.Unauthorised
    it "should not authorise a private layer if no token is given" $ do
      let noToken = Nothing
      layerAuthorisation <- LayerLib.checkPrivateLayerAuthorisation pool cache privateLayer noToken
      layerAuthorisation `shouldBe` Layer.Unauthorised

testFetchAuthorisedLayersForToken :: Pool.Pool -> Spec
testFetchAuthorisedLayersForToken pool =
  describe "testFetchAuthorisedLayersForToken" $ do
    it "should return authorised layers if token is in db" $ do
      layers <- LayerLib.fetchAuthorisedLayersForToken pool authorisedToken'
      layers `shouldBe` authorisedTokenLayers
    it "should return an empty list if token is not in db" $ do
      layers <- LayerLib.fetchAuthorisedLayersForToken pool "no-existent-token"
      layers `shouldBe` []

setupTokens :: Pool.Pool -> Token.Cache -> IO ()
setupTokens pool cache = do
  _ <- TokenLib.updateOrInsertToken pool cache authorisedToken
  _ <- TokenLib.updateOrInsertToken pool cache unauthorisedToken
  pure ()

unimplemented :: IO ()
unimplemented =
  fail "unimplemented"

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
    { Layer._layerName         = "public_layer"
    , Layer._layerSecurity     = Layer.LayerSecurityPublic
    , Layer._layerQuery        = "SELECT geojson FROM public_layer WHERE ST_Intersects(wkb_geometry, !bbox_4326!)"
    , Layer._layerLastModified = Time.parseTimeOrError True Time.defaultTimeLocale (Time.iso8601DateFormat Nothing) "2018-01-01" :: Time.UTCTime
    , Layer._layerQuantize     = 4
    , Layer._layerAlgorithms   = Map.empty
    }

privateLayer :: Layer.Layer
privateLayer =
  Layer.Layer
    { Layer._layerName         = "private_layer"
    , Layer._layerSecurity     = Layer.LayerSecurityPrivate
    , Layer._layerQuery        = "SELECT geojson FROM private_layer WHERE ST_Intersects(wkb_geometry, !bbox_4326!)"
    , Layer._layerLastModified = Time.parseTimeOrError True Time.defaultTimeLocale (Time.iso8601DateFormat Nothing) "2018-01-01" :: Time.UTCTime
    , Layer._layerQuantize     = 4
    , Layer._layerAlgorithms   = Map.empty
    }
