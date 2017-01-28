{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Routes where

import           Data.ByteString as BS
import           Data.Text       as T
import           Servant

import           Types

type LayerName = Capture "layer" Text
type Z = Capture "z" Integer
type X = Capture "x" Integer
type Y = Capture "y" Text
type YI = Capture "y" Integer

type HastileApi =    Get '[PlainText] Text
                :<|> LayerName :> ReqBody '[JSON] LayerQuery :> Post '[JSON] NoContent
                :<|> LayerName :> Z :> X :> YI :> "query" :> Get '[PlainText] Text
                :<|> LayerName :> Z :> X :> Y :> Get '[OctetStream] (Headers '[Header "Last-Modified" String] BS.ByteString)

api :: Proxy HastileApi
api = Proxy
