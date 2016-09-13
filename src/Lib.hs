{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Lib
    ( api
    , hastileService
    , ServerState (..)
    ) where

import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Data.Map
import           Data.Text                  as T
import           Hasql.Pool                 as P
import           Servant

import           SphericalMercator

type Layer = Capture "layer" Text
type Z     = Capture "z" Integer
type X     = Capture "x" Integer
type Y     = Capture "y" Integer
type HastileApi =    Layer :> Z :> X :> Y :> "query" :> Get '[PlainText] Text
                :<|> Layer :> Z :> X :> Y :> "mvt" :> Get '[PlainText] Text

-- TODO: make lenses!
data ServerState = ServerState { pool :: P.Pool
                               , stateLayers :: Map Text Text
                               }

api :: Proxy HastileApi
api = Proxy

hastileService :: ServerState -> Server HastileApi
hastileService state =
  enter (runReaderTNat state) (getQuery :<|> getTile)


getQuery :: (MonadError ServantErr m, MonadReader ServerState m) => Text -> Integer -> Integer -> Integer -> m Text
getQuery l z x y = do
  s <- ask
  let ls = stateLayers s
  case Data.Map.lookup l ls of
    Just rawQuery -> pure . escape bbox4326 $ rawQuery
    Nothing -> throwError $ err404 { errBody = "Layer not found :(" }
  where (BBox (Metres llX) (Metres llY) (Metres urX) (Metres urY)) =
          googleToBBoxM 256 (ZoomLevel z) (GoogleTileCoords x y)
        bbox4326 = pack $ "ST_Transform(ST_SetSRID(ST_MakeBox2D(\
                           \ST_MakePoint(" ++ show llX ++ ", " ++ show llY ++ ", \
                           \ST_MakePoint(" ++ show urX ++ ", " ++ show urY ++ ")) 3857) 4326)"

getTile :: Applicative m => Text -> Integer -> Integer -> Integer -> m Text
getTile _l _z _x _y = pure "I'm not implemented yet"

escape :: Text -> Text -> Text
escape bbox query = T.concat . fmap replace' . T.split (== '!') $ query
  where replace' "bbox4326" = bbox
        replace' t = t
