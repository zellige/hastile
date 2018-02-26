{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Routes where

import qualified Data.ByteString           as BS
import qualified Data.Proxy                as P
import qualified Data.Text                 as T
import           Servant

import qualified Data.Geometry.Types.Types as DGTT
import qualified Types                     as T

type LayerName = Capture "layer" T.Text
type Z = Capture "z" DGTT.ZoomLevel
type X = Capture "x" DGTT.Pixels
type Y = Capture "y" T.Text
type YI = Capture "y" DGTT.Pixels

type HastileApi =
  Get '[JSON] T.InputConfig
  :<|> ReqBody '[JSON] T.LayerRequestList :> Post '[JSON] NoContent
  :<|> LayerApi

type LayerApi =
  LayerName :>
    (
      ReqBody '[JSON] T.LayerSettings :> Post '[JSON] NoContent
      :<|> Z :> X :> HastileContentApi
    )

type HastileContentApi =
       YI :> "query" :> Get '[PlainText] T.Text
  :<|> Y             :> Get '[T.MapboxVectorTile, T.AlreadyJSON] (Headers '[Header "Last-Modified" String] BS.ByteString)

hastileApi :: P.Proxy HastileApi
hastileApi = P.Proxy
