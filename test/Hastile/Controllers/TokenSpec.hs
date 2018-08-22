{-# LANGUAGE OverloadedStrings #-}

module Hastile.Controllers.TokenSpec where

import qualified Data.IORef                as IORef
import qualified Data.LruCache             as LRU
import qualified Data.LruCache.IO          as LRUIO
import           Test.Hspec                (Spec, describe, it, runIO, shouldBe)

import qualified Hastile.Controllers.Token as Token
import qualified Hastile.Types.Token       as Token

spec :: Spec
spec =
  testCache

testCache :: Spec
testCache =
  describe "testCache" $ do
    cache <- runIO $ LRUIO.newLruHandle 1
    _ <- runIO $ LRUIO.cached cache exampleToken (pure exampleLayers)
    it "should insert a token into the cache" $
      checkCache cache exampleToken exampleLayers
    it "should update the authorised layers of a token in the cache" $ do
      let updatedToken = Token.TokenAuthorisation exampleToken updatedExampleLayers
      Token.updateCache cache updatedToken
      checkCache cache exampleToken updatedExampleLayers
    it "should deauthorise a token in the cache" $ do
      let updatedToken = Token.unauthorisedToken exampleToken
      Token.updateCache cache updatedToken
      checkCache cache exampleToken []

checkCache :: LRUIO.LruHandle Token.Token Token.Layers -> Token.Token -> Token.Layers -> IO ()
checkCache cache token expectedLayers = do
  let (LRUIO.LruHandle ref) = cache
  unwrappedCache <- IORef.readIORef ref
  case LRU.lookup token unwrappedCache of
    Just (foundLayers, _) ->
      foundLayers `shouldBe` expectedLayers
    Nothing ->
      fail "did not find token"

exampleToken :: Token.Token
exampleToken = "abcd"

exampleLayers :: Token.Layers
exampleLayers = ["layer1", "layer2"]

updatedExampleLayers :: Token.Layers
updatedExampleLayers = ["layer1", "layer3"]
