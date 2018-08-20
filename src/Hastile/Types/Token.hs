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
import           Data.Aeson                 as Aeson
import qualified Data.Foldable              as Foldable
import qualified Data.Functor.Contravariant as Contravariant
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

instance Aeson.FromJSON TokenAuthorisation where
  parseJSON = withObject "TokenAuthorisation" $ \o -> TokenAuthorisation
    <$> o .: "token"
    <*> o .: "layers"

instance Aeson.ToJSON TokenAuthorisation where
  toJSON ls = object
    [ "token"  .= _token ls
    , "layers" .= _layers ls
    ]

layersDecoder :: Hasql.Decoders.Row Layers
layersDecoder =
  Hasql.Decoders.value
    (Hasql.Decoders.array $
      Hasql.Decoders.arrayDimension Control.Monad.replicateM $ Hasql.Decoders.arrayValue Hasql.Decoders.text)

tokenDecoder :: Hasql.Decoders.Row TokenAuthorisation
tokenDecoder = TokenAuthorisation
  <$> Hasql.Decoders.value Hasql.Decoders.text
  <*> Hasql.Decoders.value
      (Hasql.Decoders.array $
         Hasql.Decoders.arrayDimension Control.Monad.replicateM $ Hasql.Decoders.arrayValue Hasql.Decoders.text)

tokenEncoder :: Hasql.Encoders.Params TokenAuthorisation
tokenEncoder =
  Contravariant.contramap _token (Hasql.Encoders.value Hasql.Encoders.text)
  <> Contravariant.contramap _layers (Hasql.Encoders.value $ Hasql.Encoders.array (Hasql.Encoders.arrayDimension Foldable.foldl' (Hasql.Encoders.arrayValue Hasql.Encoders.text)))
