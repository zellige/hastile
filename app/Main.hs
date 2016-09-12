{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Exception.Base
import           Data.Aeson
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Maybe (fromMaybe)
import           Data.Time (NominalDiffTime)
import           Hasql.Pool                  as P
import           Lib
import qualified Network.Wai.Handler.Warp    as Warp
import           Options.Generic
import           Servant

data CmdLine = CmdLine { configFile :: FilePath
                       }
               deriving Generic
instance ParseRecord CmdLine

data Config = Config { pgConnection :: String
                     , port :: Maybe Int
                     , pgPoolSize :: Maybe Int
                     , pgTimeout :: Maybe NominalDiffTime
                     } deriving (Show, Generic)
instance FromJSON Config where

main :: IO ()
main = getRecord "hastile" >>= doIt

doIt :: CmdLine -> IO ()
doIt cmdLine = do
  configBs <- BSL.readFile $ configFile cmdLine
  case (decode configBs) of
    Just config -> doItWithConfig config
    Nothing -> putStrLn $ "Failed to parse config file " ++ configFile cmdLine

doItWithConfig :: Config -> IO ()
doItWithConfig config = do
  let pgPoolSize' = fromMaybe 10 $ pgPoolSize config
      pgTimeout'  = fromMaybe 1 $ pgTimeout config
      port'    = fromMaybe 8080 $ port config
  bracket (P.acquire (pgPoolSize', pgTimeout', BS.pack . pgConnection $ config))
          (P.release)
          (\pool -> Warp.run port'. serve api $ hastileService pool)
