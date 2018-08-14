{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hastile.Types.Mime where

import qualified Data.ByteString      as BS
import           Data.ByteString.Lazy (ByteString, fromStrict)
import           Data.Typeable
import qualified Network.HTTP.Media   as HM
import           Servant

data AlreadyJSON deriving Typeable

instance Accept AlreadyJSON where
    contentType _ = "application" HM.// "json"

instance MimeRender AlreadyJSON Data.ByteString.Lazy.ByteString where
    mimeRender _ = id

instance MimeRender AlreadyJSON BS.ByteString where
    mimeRender _ = fromStrict

data MapboxVectorTile deriving Typeable

instance Accept MapboxVectorTile where
    contentType _ = "application" HM.// "vnd.mapbox-vector-tile"

instance MimeRender MapboxVectorTile Data.ByteString.Lazy.ByteString where
    mimeRender _ = id

instance MimeRender MapboxVectorTile BS.ByteString where
    mimeRender _ = fromStrict
