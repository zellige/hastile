{-# LANGUAGE OverloadedStrings #-}

module Hastile.Lib.TokenSpec where

import qualified Control.Monad.IO.Class as IOClass
import qualified Data.ByteString.Char8  as ByteString
import qualified Data.IORef             as IORef
import qualified Data.LruCache          as LRU
import qualified Data.LruCache.IO       as LRUIO
import qualified Hasql.Pool             as Pool
import           Test.Hspec             (Spec, before, describe, it, runIO,
                                         shouldBe)

import qualified Hastile.DB.Token       as TokenDB
import qualified Hastile.Lib.Token      as TokenLib
import qualified Hastile.Types.Token    as Token

spec :: Spec
spec = do
  settings <- runIO $ readFile "db/env/test"
  pool <- runIO $ Pool.acquire (1, 10, ByteString.pack settings)
  testUpdateOrInsert pool
  testDelete pool
  testCache pool
  runIO $ Pool.release pool

beforeEach :: IOClass.MonadIO m => Pool.Pool -> m Token.Cache
beforeEach pool = do
  _ <- IOClass.liftIO $ TokenDB.clearTokens pool
  IOClass.liftIO $ LRUIO.newLruHandle 1

testUpdateOrInsert :: Pool.Pool -> Spec
testUpdateOrInsert pool =
  before (beforeEach pool) $
    describe "testUpdateOrInsert" $ do
      it "should insert a token" $ \cache -> do
        _ <- TokenLib.updateOrInsertToken pool cache exampleTokenAuthorisation
        foundLayers <- TokenDB.getToken pool exampleToken
        foundLayers `shouldBe` Right exampleLayers
        checkCache cache exampleToken exampleLayers
      it "should update a token" $ \cache -> do
        _ <- TokenLib.updateOrInsertToken pool cache updatedTokenAuthorisation
        foundLayers <- TokenDB.getToken pool exampleToken
        foundLayers `shouldBe` Right updatedExampleLayers
        checkCache cache exampleToken updatedExampleLayers

testDelete :: Pool.Pool -> Spec
testDelete pool =
  before (beforeEach pool) $
    describe "testDelete" $ do
      it "should delete a token" $ \cache -> do
        _ <- TokenLib.updateOrInsertToken pool cache exampleTokenAuthorisation
        result <- TokenLib.deleteToken pool cache exampleToken
        result `shouldBe` Right "OK"
        foundLayers <- TokenDB.getToken pool exampleToken
        foundLayers `shouldBe` Left "SessionError (QueryError \"SELECT layers FROM tokens WHERE token LIKE $1;\" [\"\\\"example\\\"\"] (ResultError (UnexpectedAmountOfRows 0)))"
        checkCache cache exampleToken []
      it "should fail to delete a non-existent token" $ \cache -> do
        result <- TokenLib.deleteToken pool cache exampleToken
        result `shouldBe` Left "Delete failed"

testCache :: Pool.Pool ->  Spec
testCache pool =
  before (beforeEach pool) $
    describe "testCache" $ do
      it "should insert a token into the cache" $ \cache -> do
        _ <- IOClass.liftIO $ LRUIO.cached cache exampleToken (pure exampleLayers)
        checkCache cache exampleToken exampleLayers
      it "should update the authorised layers of a token in the cache" $ \cache -> do
        TokenLib.updateCache cache updatedTokenAuthorisation
        checkCache cache exampleToken updatedExampleLayers
      it "should deauthorise a token in the cache" $ \cache -> do
        let updatedToken = Token.unauthorisedToken exampleToken
        TokenLib.updateCache cache updatedToken
        checkCache cache exampleToken []

checkCache :: Token.Cache -> Token.Token -> Token.Layers -> IO ()
checkCache cache token expectedLayers = do
  let (LRUIO.LruHandle ref) = cache
  unwrappedCache <- IORef.readIORef ref
  case LRU.lookup token unwrappedCache of
    Just (foundLayers, _) ->
      foundLayers `shouldBe` expectedLayers
    Nothing ->
      fail "did not find token"

exampleTokenAuthorisation :: Token.TokenAuthorisation
exampleTokenAuthorisation =
  Token.TokenAuthorisation exampleToken exampleLayers

updatedTokenAuthorisation :: Token.TokenAuthorisation
updatedTokenAuthorisation =
  Token.TokenAuthorisation exampleToken updatedExampleLayers

exampleToken :: Token.Token
exampleToken = "example"

exampleLayers :: Token.Layers
exampleLayers = ["layer1", "layer2"]

updatedExampleLayers :: Token.Layers
updatedExampleLayers = ["layer1", "layer3"]
