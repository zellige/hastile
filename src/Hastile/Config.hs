{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Config where

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Data.Either                as DataEither
import qualified Data.Map.Strict            as DataMapStrict
import qualified Data.Maybe                 as DataMaybe
import qualified Data.Text.Encoding         as TextEncoding
import qualified Hasql.Pool                 as HasqlPool
import           System.Exit                as SystemExit

import qualified Hastile.Lib.Layer          as LibLayer
import qualified Hastile.Types.Config       as Config

getConfig :: FilePath -> IO Config.Config
getConfig cfgFile = do
  configBs <- ByteStringLazyChar8.readFile cfgFile
  case Aeson.eitherDecode configBs of
    Left e -> do
      putStrLn $ "In file: " <> cfgFile <> "\nError: " <> e
      exitWith (SystemExit.ExitFailure 2)
    Right config -> pure (addDefaults config)

addDefaults :: Config.InputConfig -> Config.Config
addDefaults Config.InputConfig{..} =
  Config.Config
    (DataMaybe.fromMaybe "development" _inputConfigEnvironment)
    (DataMaybe.fromMaybe "stdout" _inputConfigAccessLog)
    (DataMaybe.fromMaybe "stdout" _inputConfigAppLog)
    _inputConfigPgConnection
    (DataMaybe.fromMaybe 10 _inputConfigPgPoolSize)
    (DataMaybe.fromMaybe 1 _inputConfigPgTimeout)
    (DataMaybe.fromMaybe 8080 _inputConfigPort)
    (DataMaybe.fromMaybe 1000 _inputConfigTokenCacheSize)
    _inputConfigLayers
    (DataMaybe.fromMaybe 128 _inputConfigTileBuffer)

checkConfig :: FilePath -> Config.Config -> IO ()
checkConfig cfgFile Config.Config{..} = do
  pool <- HasqlPool.acquire (_configPgPoolSize, _configPgTimeout, TextEncoding.encodeUtf8 _configPgConnection)
  let layers = DataMapStrict.elems _configLayers
  result <- mapM (LibLayer.checkLayerExists pool) layers
  case DataEither.lefts result of
    [] ->
      pure ()
    errs -> do
      putStrLn $ "In file: " <> cfgFile <> "\nError: "
      mapM_ (\err -> putStrLn $ "  " <> err) errs
