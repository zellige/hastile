{-# LANGUAGE OverloadedStrings #-}

module Hastile.Types.Time where

import           Data.Monoid ((<>))
import qualified Data.Text   as Text
import qualified Data.Time   as Time

lastModified :: Time.UTCTime -> Text.Text
lastModified utcTime = Text.dropEnd 3 (Text.pack rfc822Str) <> "GMT"
  where
    rfc822Str = Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat utcTime
