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
import qualified Data.Functor.Contravariant as FC
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE

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

tokenDecoder :: HD.Row Token
tokenDecoder = Token
  <$> HD.value HD.text
  <*> HD.value
      (HD.array $
         HD.arrayDimension Control.Monad.replicateM $ HD.arrayValue HD.text)

tokenEncoder :: HE.Params Token
tokenEncoder =
  FC.contramap _tokenToken (HE.value HE.text) <>
  FC.contramap _tokenLayers (HE.value $ HE.array (HE.arrayDimension Foldable.foldl' (HE.arrayValue HE.text)))
