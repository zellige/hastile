{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception.Base
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Foldable
import           Data.Map
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           GHC.Conc
import           Hasql.Pool                  as P
import           Network.Wai
import qualified Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Cors
import           Options.Generic
import           Servant
import           STMContainers.Map           as STM
import           System.Exit

import           Lib
import           Types

main :: IO ()
main = getRecord "hastile" >>= doIt

doIt :: CmdLine -> IO ()
doIt cmdLine = do
  let cfgFile = configFile cmdLine
  configBs <- LBS.readFile cfgFile
  case eitherDecode configBs of
    Left e -> do
      putStrLn $ "In file: " <> cfgFile <> "\nError: " <> e
      exitWith (ExitFailure 2)
    Right inputConfig -> doItWithConfig cfgFile $ addDefaults inputConfig

doItWithConfig :: FilePath -> Config -> IO ()
doItWithConfig cfgFile config@Config{..} = do
  layers <- atomically STM.new :: IO (STM.Map Text Layer)
  forM_ (Data.Map.toList _configLayers) $ \(k, v) -> atomically $ STM.insert v k layers
  bracket (P.acquire (_configPgPoolSize, _configPgTimeout, encodeUtf8 _configPgConnection))
    P.release $
      \p -> getWarp _configPort . serve api $ hastileService (ServerState p _configMapnikInputPlugins cfgFile config layers)
  pure ()

getWarp :: Warp.Port -> Network.Wai.Application -> IO ()
getWarp port' = Warp.run port' . cors (const $ Just policy)
           where
             policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

addDefaults :: InputConfig -> Config
addDefaults ic = Config (_inputConfigPgConnection ic)
                        (fromMaybe 10 $ _inputConfigPgPoolSize ic)
                        (fromMaybe 1 $ _inputConfigPgTimeout ic)
                        (fromMaybe "/usr/local/lib/mapnik/input" $ _inputConfigMapnikInputPlugins ic)
                        (fromMaybe 8080 $ _inputConfigPort ic)
                        (_inputConfigLayers ic)
                        (fromMaybe 128 $ _inputConfigTileBuffer ic)
