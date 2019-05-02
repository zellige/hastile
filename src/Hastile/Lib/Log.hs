{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Hastile.Lib.Log (
  logErrors
) where


import qualified Data.List as DataList
import qualified Katip

logErrors :: FilePath -> [String] -> Katip.KatipContextT IO ()
logErrors cfgFile errs =
  $(Katip.logTM) Katip.ErrorS $ Katip.logStr $
    "Error in file: " <> cfgFile <> separator <> DataList.intercalate separator errs
  where separator = "\n  "
