{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hastile.Types.Token where

import qualified Control.Monad
import           Data.Aeson     as Aeson
import qualified Data.Text      as Text
import qualified Hasql.Decoders as HD

data Token = Token
  { _tokenToken  :: Text.Text
  , _tokenLayers :: [Text.Text]
  } deriving (Show, Eq)

instance Aeson.FromJSON Token where
  parseJSON = withObject "Token" $ \o -> Token
    <$> o .: "token"
    <*> o .: "layer"

instance Aeson.ToJSON Token where
  toJSON ls = object
    [ "token"  .= _tokenToken ls
    , "layers" .= _tokenLayers ls
    ]

tokenDecoder :: HD.Row Token
tokenDecoder = Token
  <$> HD.value HD.text
  <*> (HD.value $ HD.array $ HD.arrayDimension Control.Monad.replicateM $ HD.arrayValue HD.text)
