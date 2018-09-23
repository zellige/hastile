{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hastile.Types.Tile where

import qualified Data.Functor.Contravariant as Contravariant
import           Data.Monoid                ((<>))
import qualified Hasql.Encoders             as HasqlEncoders

-- SW and NE points given as W,S,E,N
data BBox a = BBox
  { _bboxLlx :: a
  , _bboxLly :: a
  , _bboxUrx :: a
  , _bboxUry :: a
  } deriving (Show, Eq, Functor)

newtype Metres = Metres Double deriving (Show, Eq, Num, Floating, Fractional, Ord)

metreValue :: HasqlEncoders.Value Metres
metreValue =
  Contravariant.contramap metreTodouble HasqlEncoders.float8
  where
    metreTodouble (Metres double) = double

bboxEncoder :: HasqlEncoders.Params (BBox Metres)
bboxEncoder =
  Contravariant.contramap _bboxLlx (HasqlEncoders.param metreValue)
  <> Contravariant.contramap _bboxLly (HasqlEncoders.param metreValue)
  <> Contravariant.contramap _bboxUrx (HasqlEncoders.param metreValue)
  <> Contravariant.contramap _bboxUry (HasqlEncoders.param metreValue)
