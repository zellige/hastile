{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Exception.Base
import qualified Data.ByteString.Char8    as BS
import           Data.Maybe (fromMaybe)
import           Hasql.Pool               as P
import           Lib
import qualified Network.Wai.Handler.Warp as Warp
import           Options.Generic
import           Servant

data CmdLine = CmdLine { pgConfig :: FilePath
                       , port :: Maybe Int
                       }
               deriving Generic
instance ParseRecord CmdLine

main :: IO ()
main = getRecord "hastile" >>= doIt

doIt :: CmdLine -> IO ()
doIt cmdLine = do
  let poolSize = 10
      timeout  = 1
      port'     = fromMaybe 8080 $ port cmdLine
  -- TODO: read file better
  connString <- fmap (head . BS.split '\n') . BS.readFile $ pgConfig cmdLine
  bracket (P.acquire (poolSize, timeout, connString))
          (P.release)
          (\pool -> Warp.run port'. serve api $ hastileService pool)
