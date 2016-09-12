{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Lib
    ( api
    , hastileService
    ) where

import           Control.Monad.Reader.Class
import           Data.Text
import           Hasql.Pool                 as P
import           Servant

import           SphericalMercator

type Layer = Capture "layer" String
type Z     = Capture "z" Integer
type X     = Capture "x" Integer
type Y     = Capture "y" Integer
type HastileApi =    Layer :> Z :> X :> Y :> "query" :> Get '[PlainText] Text
                :<|> Layer :> Z :> X :> Y :> "mvt" :> Get '[PlainText] Text

api :: Proxy HastileApi
api = Proxy

hastileService :: Pool -> Server HastileApi
hastileService pool =
  enter (runReaderTNat pool) (getQuery :<|> getTile)


getQuery :: MonadReader P.Pool m => String -> Integer -> Integer -> Integer -> m Text
getQuery _l z x y = pure $ Data.Text.concat [queryPre, bboxSql, queryPost]
  where queryPre  = "SELECT linkid, name, load_total_24_2way, load_commveh_24_2way, \
                       \ST_AsGeoJSON(wkb_geometry) as the_geom_geojson FROM \"traffic_per_v5b\" \
                     \WHERE ST_Intersects(wkb_geometry, "
        queryPost = pack ")"
        (BBox (Metres llX) (Metres llY) (Metres urX) (Metres urY)) =
          googleToBBoxM 256 (ZoomLevel z) (GoogleTileCoords x y)
        bboxSql   = pack $ "ST_Transform(ST_SetSRID(ST_MakeBox2D(\
                           \ST_MakePoint(" ++ show llX ++ ", " ++ show llY ++ ", \
                           \ST_MakePoint(" ++ show urX ++ ", " ++ show urY ++ ")) 3857) 4326)"

getTile :: Applicative m => String -> Integer -> Integer -> Integer -> m Text
getTile _l _z _x _y = pure "I'm not implemented yet"
