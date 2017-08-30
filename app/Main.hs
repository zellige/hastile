{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Control.Exception.Base      as CE
import qualified Data.Aeson                  as A
import qualified Data.ByteString.Lazy.Char8  as LBS
import qualified Data.Foldable               as F
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid
import           Data.Text.Encoding
import           GHC.Conc
import qualified Hasql.Pool                  as P
import           Network.Wai
import qualified Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Cors
import           Options.Generic
import qualified Servant                     as S
import           STMContainers.Map           as STM
import qualified System.Exit                 as SE

import           Lib
import           Types

main :: IO ()
main = getRecord "hastile" >>= doIt

doIt :: CmdLine -> IO ()
doIt cmdLine = do
  let cfgFile = configFile cmdLine
  configBs <- LBS.readFile cfgFile
  case A.eitherDecode configBs of
    Left e -> do
      putStrLn $ "In file: " <> cfgFile <> "\nError: " <> e
      SE.exitWith (SE.ExitFailure 2)
    Right inputConfig -> doItWithConfig cfgFile $ addDefaults inputConfig

doItWithConfig :: FilePath -> Config -> IO ()
doItWithConfig cfgFile config@Config{..} = do
  layers <- atomically STM.new :: IO (STM.Map Text Layer)
  F.forM_ (M.toList _configLayers) $ \(k, v) -> atomically $ STM.insert v k layers
  CE.bracket (P.acquire (_configPgPoolSize, _configPgTimeout, encodeUtf8 _configPgConnection))
    P.release $
      \p -> getWarp _configPort . S.serve api $ hastileService (ServerState p _configMapnikInputPlugins cfgFile config layers)
  pure ()

getWarp :: Warp.Port -> Network.Wai.Application -> IO ()
getWarp port' = Warp.run port' . cors (const $ Just policy)
           where
             policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

addDefaults :: InputConfig -> Config
addDefaults InputConfig{..} =
  Config
    _inputConfigPgConnection
    (fromMaybe 10 _inputConfigPgPoolSize)
    (fromMaybe 1 _inputConfigPgTimeout)
    (fromMaybe "/usr/local/lib/mapnik/input" _inputConfigMapnikInputPlugins)
    (fromMaybe 8080 _inputConfigPort)
    _inputConfigLayers
    (fromMaybe 128 _inputConfigTileBuffer)
