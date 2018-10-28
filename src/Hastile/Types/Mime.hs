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

import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy (ByteString, fromStrict)
import           Data.Typeable
import qualified Network.HTTP.Media   as HTTPMedia
import           Servant

data AlreadyJSON deriving Typeable

instance Accept AlreadyJSON where
    contentType _ = "application" HTTPMedia.// "json"

instance MimeRender AlreadyJSON ByteStringLazy.ByteString where
    mimeRender _ = id

instance MimeRender AlreadyJSON ByteString.ByteString where
    mimeRender _ = ByteStringLazy.fromStrict

data MapboxVectorTile deriving Typeable

instance Accept MapboxVectorTile where
    contentType _ = "application" HTTPMedia.// "vnd.mapbox-vector-tile"

instance MimeRender MapboxVectorTile ByteStringLazy.ByteString where
    mimeRender _ = id

instance MimeRender MapboxVectorTile ByteString.ByteString where
    mimeRender _ = ByteStringLazy.fromStrict
