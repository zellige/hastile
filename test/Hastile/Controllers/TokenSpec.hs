{-# LANGUAGE OverloadedStrings #-}

module Hastile.Controllers.TokenSpec where

import qualified Data.IORef          as IORef
import qualified Data.LruCache       as LRU
import qualified Data.LruCache.IO    as LRUIO
import           Test.Hspec          (Spec, describe, it, runIO, shouldBe)

import qualified Hastile.Types.Token as Token

spec :: Spec
spec =
  testCache

testCache :: Spec
testCache =
  describe "testCache" $ do
    cache <- runIO $ LRUIO.newLruHandle 1
    _ <- runIO $ LRUIO.cached cache sampleToken (pure sampleLayers)
    it "should insert a token into the cache" $ do
      let (LRUIO.LruHandle ref) = cache
      unwrappedCache <- IORef.readIORef ref
      case LRU.lookup sampleToken unwrappedCache of
        Just (foundLayers, _) ->
          foundLayers `shouldBe` sampleLayers
        Nothing ->
          fail "did not find token"

sampleToken :: Token.Token
sampleToken = "abcd"

sampleLayers :: Token.Layers
sampleLayers = ["layer1", "layer2"]
