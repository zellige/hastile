{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Hastile.Routes where

import qualified Data.ByteString as ByteString
import qualified Data.Geometry.Types.Geography as GeometryTypesGeography
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Hastile.Types.Config as Config
import qualified Hastile.Types.Layer as Layer
import qualified Hastile.Types.Mime as Mime
import qualified Hastile.Types.Tile as Tile
import qualified Hastile.Types.Token as Token
import qualified Hastile.Types.App as App
import Servant

type LayerName = Capture "layer" Text.Text

type Z = Capture "z" GeometryTypesGeography.ZoomLevel

type X = Capture "x" GeometryTypesGeography.Pixels

type Y = Capture "y" Text.Text

type PublicHastileApi =
  Get '[JSON] Config.InputConfig
    :<|> Get '[HTML] App.RawHtml
    :<|> LayerApi

type AuthenticatedHastileApi =
  Get '[JSON] Config.InputConfig
    :<|> TokenApi
    :<|> LayerApi

type TokenApi =
  "token"
    :> ( Get '[JSON] [Token.TokenAuthorisation]
           :<|> Capture "token" Text.Text :> Get '[JSON] Token.Layers
           :<|> ReqBody '[JSON] Token.TokenAuthorisation :> Post '[JSON] Text.Text
           :<|> Capture "token" Text.Text :> Delete '[JSON] Text.Text
       )

type LayerApi =
  ReqBody '[JSON] Layer.LayerRequestList :> Post '[JSON] NoContent
    :<|> ( LayerName
             :> ( ReqBody '[JSON] Layer.LayerSettings :> Post '[JSON] NoContent
                    :<|> Z :> X :> HastileContentApi
                    :<|> Get '[JSON] Tile.Tile
                )
         )

type HastileContentApi =
  Y :> QueryParam "token" Text.Text :> Servant.Header "If-Modified-Since" Text.Text :> Get '[Mime.MapboxVectorTile, Mime.AlreadyJSON] (Headers '[Header "Last-Modified" Text.Text, Header "Expires" Text.Text] ByteString.ByteString)

publicHastileApi :: Proxy.Proxy PublicHastileApi
publicHastileApi = Proxy.Proxy

authenticatedHastileApi :: Proxy.Proxy AuthenticatedHastileApi
authenticatedHastileApi = Proxy.Proxy
