{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hastile.Types.Tile where

import qualified Data.Functor.Contravariant as Contravariant
import           Data.Monoid                ((<>))
import qualified Hasql.Encoders

-- SW and NE points given as W,S,E,N
data BBox a = BBox
  { _bboxLlx :: a
  , _bboxLly :: a
  , _bboxUrx :: a
  , _bboxUry :: a
  } deriving (Show, Eq, Functor)

newtype Metres = Metres Double deriving (Show, Eq, Num, Floating, Fractional, Ord)

metreValue :: Hasql.Encoders.Value Metres
metreValue =
  Contravariant.contramap metreTodouble Hasql.Encoders.float8
  where metreTodouble (Metres double) = double

bboxEncoder :: Hasql.Encoders.Params (BBox Metres)
bboxEncoder =
  Contravariant.contramap _bboxLlx (Hasql.Encoders.value metreValue)
  <> Contravariant.contramap _bboxLly (Hasql.Encoders.value metreValue)
  <> Contravariant.contramap _bboxUrx (Hasql.Encoders.value metreValue)
  <> Contravariant.contramap _bboxUry (Hasql.Encoders.value metreValue)
