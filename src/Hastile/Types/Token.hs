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

data Token = Token
  { _tokenToken  :: Text.Text
  , _tokenLayers :: [Text.Text]
  } deriving (Show, Eq)

instance Aeson.FromJSON Token where
  parseJSON = withObject "Token" $ \o -> Token
    <$> o .: "token"
    <*> o .: "layers"

instance Aeson.ToJSON Token where
  toJSON ls = object
    [ "token"  .= _tokenToken ls
    , "layers" .= _tokenLayers ls
    ]

tokenDecoder :: Hasql.Decoders.Row Token
tokenDecoder = Token
  <$> Hasql.Decoders.value Hasql.Decoders.text
  <*> Hasql.Decoders.value
      (Hasql.Decoders.array $
         Hasql.Decoders.arrayDimension Control.Monad.replicateM $ Hasql.Decoders.arrayValue Hasql.Decoders.text)

tokenEncoder :: Hasql.Encoders.Params Token
tokenEncoder =
  Contravariant.contramap _tokenToken (Hasql.Encoders.value Hasql.Encoders.text) <>
  Contravariant.contramap _tokenLayers (Hasql.Encoders.value $ Hasql.Encoders.array (Hasql.Encoders.arrayDimension Foldable.foldl' (Hasql.Encoders.arrayValue Hasql.Encoders.text)))
