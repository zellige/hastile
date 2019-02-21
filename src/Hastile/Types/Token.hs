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

module Hastile.Types.Token where

import qualified Control.Monad
import qualified Data.Aeson                 as Aeson
import qualified Data.Foldable              as Foldable
import qualified Data.Functor.Contravariant as Contravariant
import qualified Data.LruCache.IO           as LRUIO
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import qualified Hasql.Decoders
import qualified Hasql.Encoders

data TokenAuthorisation = TokenAuthorisation
  { _token  :: Token
  , _layers :: Layers
  } deriving (Show, Eq)

type Token = Text.Text

type Layers = [Layer]

type Layer = Text.Text

type Cache = LRUIO.LruHandle Token Layers

instance Aeson.FromJSON TokenAuthorisation where
  parseJSON = Aeson.withObject "TokenAuthorisation" $ \o -> TokenAuthorisation
    <$> o Aeson..: "token"
    <*> o Aeson..: "layers"

instance Aeson.ToJSON TokenAuthorisation where
  toJSON ls = Aeson.object
    [ "token"  Aeson..= _token ls
    , "layers" Aeson..= _layers ls
    ]

layersDecoder :: Hasql.Decoders.Row Layers
layersDecoder =
  Hasql.Decoders.column
    (Hasql.Decoders.array $
      Hasql.Decoders.dimension Control.Monad.replicateM $ Hasql.Decoders.element Hasql.Decoders.text)

tokenDecoder :: Hasql.Decoders.Row TokenAuthorisation
tokenDecoder = TokenAuthorisation
  <$> Hasql.Decoders.column Hasql.Decoders.text
  <*> Hasql.Decoders.column
      (Hasql.Decoders.array $
         Hasql.Decoders.dimension Control.Monad.replicateM $ Hasql.Decoders.element Hasql.Decoders.text)

tokenEncoder :: Hasql.Encoders.Params TokenAuthorisation
tokenEncoder =
  Contravariant.contramap _token (Hasql.Encoders.param Hasql.Encoders.text)
  <> Contravariant.contramap _layers (Hasql.Encoders.param $ Hasql.Encoders.array (Hasql.Encoders.dimension Foldable.foldl' (Hasql.Encoders.element Hasql.Encoders.text)))

unauthorisedToken :: Token -> TokenAuthorisation
unauthorisedToken token = TokenAuthorisation token []
