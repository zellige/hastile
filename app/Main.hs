{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Control.Exception.Base      as CE
import qualified Data.Foldable               as F
import qualified Data.Map                    as M
import qualified Data.Text.Encoding          as DTE
import           GHC.Conc
import qualified Hasql.Pool                  as P
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Middleware.Cors as W
import qualified Options.Generic             as OG
import           STMContainers.Map           as STM

import qualified Config                      as HC
import qualified Server                      as HS
import qualified Types                       as HT

main :: IO ()
main = OG.getRecord "hastile" >>= doIt

doIt :: HT.CmdLine -> IO ()
doIt cmdLine = do
  let cfgFile = HT.configFile cmdLine
  config <- HC.getConfig cfgFile
  doItWithConfig cfgFile config

doItWithConfig :: FilePath -> HT.Config -> IO ()
doItWithConfig cfgFile config@HT.Config{..} = do
  layers <- atomically STM.new :: IO (STM.Map OG.Text HT.Layer)
  F.forM_ (M.toList _configLayers) $ \(k, v) -> atomically $ STM.insert (HT.layerDetailsToLayer k v) k layers
  CE.bracket
    (P.acquire (_configPgPoolSize, _configPgTimeout, DTE.encodeUtf8 _configPgConnection))
    P.release
    (\p -> getWarp _configPort (HS.runServer (HT.ServerState p _configMapnikInputPlugins cfgFile config layers)))
  pure ()

getWarp :: W.Port -> W.Application -> IO ()
getWarp port' = W.run port' . W.cors (const $ Just policy)
  where
    policy = W.simpleCorsResourcePolicy { W.corsRequestHeaders = ["Content-Type"] }
