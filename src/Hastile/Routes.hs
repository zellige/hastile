{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Routes where

import qualified Data.ByteString           as BS
import qualified Data.Geometry.Types.Types as DGTT
import qualified Data.Proxy                as P
import qualified Data.Text                 as T
import           Servant

import qualified Hastile.Types.Config      as Config
import qualified Hastile.Types.Layer       as Layer
import qualified Hastile.Types.Mime        as Mime
import qualified Hastile.Types.Token       as Token

type LayerName = Capture "layer" T.Text
type Z = Capture "z" DGTT.ZoomLevel
type X = Capture "x" DGTT.Pixels
type Y = Capture "y" T.Text
type YI = Capture "y" DGTT.Pixels

type HastileApi =
  Get '[JSON] Config.InputConfig
  :<|> ReqBody '[JSON] Layer.LayerRequestList :> Post '[JSON] NoContent
  :<|> TokenApi
  :<|> LayerApi

type TokenApi =
  "token" :> Get '[JSON] [Token.Token]

type LayerApi =
  LayerName :>
    (
      ReqBody '[JSON] Layer.LayerSettings :> Post '[JSON] NoContent
      :<|> Z :> X :> HastileContentApi
    )

type HastileContentApi =
       YI :> "query" :> Get '[PlainText] T.Text
  :<|> Y             :> Servant.Header "If-Modified-Since" T.Text :> Get '[Mime.MapboxVectorTile, Mime.AlreadyJSON] (Headers '[Header "Last-Modified" T.Text] BS.ByteString)

hastileApi :: P.Proxy HastileApi
hastileApi = P.Proxy
