{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hastile.Types.Layer.Security where

import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as AesonTypes

data LayerSecurity = Public | Private deriving (Eq)

data LayerAuthorisation = Authorised | Unauthorised deriving (Show, Eq)

instance Show LayerSecurity where
  show Public  = "public"
  show Private = "private"

instance Aeson.FromJSON LayerSecurity where
  parseJSON = AesonTypes.withText "LayerSecurity" $ \case
    "public"  -> pure Public
    "private" -> pure Private
    _         -> fail "Unknown layer security"

instance Aeson.ToJSON LayerSecurity where
  toJSON algo =
    Aeson.String $ case algo of
      Public  -> "public"
      Private -> "private"
