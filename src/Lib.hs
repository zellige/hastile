{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( api
    , hastileService
    ) where

import Control.Monad.Reader.Class
import Data.Text
import Hasql.Pool               as P
--import Mapnik                   as M
import Servant

type HastileApi =    Capture "x" Double
                  :> Capture "y" Double
                  :> Capture "z" Double
                  :> Get '[PlainText] Text

api :: Proxy HastileApi
api = Proxy

hastileService :: Pool -> Server HastileApi
hastileService pool x y z =
  enter (runReaderTNat pool) $ getTile x y z

getTile :: MonadReader P.Pool m => Double -> Double -> Double -> m Text
getTile x y z = return . pack $ "x: " ++ show x ++ "y: " ++ show y ++ "z: " ++ show z
