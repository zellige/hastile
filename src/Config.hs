{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Config where

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Maybe                 as M
import           Data.Monoid
import           System.Exit

import qualified Types                      as HT

getConfig :: FilePath -> IO HT.Config
getConfig cfgFile = do
  configBs <- LBS.readFile cfgFile
  case A.eitherDecode configBs of
    Left e -> do
      putStrLn $ "In file: " <> cfgFile <> "\nError: " <> e
      exitWith (ExitFailure 2)
    Right config -> pure (addDefaults config)

addDefaults :: HT.InputConfig -> HT.Config
addDefaults HT.InputConfig{..} =
  HT.Config
    _inputConfigPgConnection
    (M.fromMaybe 10 _inputConfigPgPoolSize)
    (M.fromMaybe 1 _inputConfigPgTimeout)
    (M.fromMaybe "/usr/local/lib/mapnik/input" _inputConfigMapnikInputPlugins)
    (M.fromMaybe 8080 _inputConfigPort)
    _inputConfigLayers
    (M.fromMaybe 128 _inputConfigTileBuffer)
