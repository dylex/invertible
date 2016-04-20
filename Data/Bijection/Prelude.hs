-- |
-- The bidirectional \"Prelude\", which re-exports various bijections similar to functions from "Prelude".
-- Most \"un\"-functions are left out for obvious reasons.
{-# LANGUAGE Safe #-}
module Data.Bijection.Prelude
  ( module Data.Bijection.Type

  , enum

  , const
  , flip
  , id
  , (.)
  , not
  , fst
  , snd
  , curry
  , cons
  , uncons
  , fmap
  , (<$>)
  , map
  , reverse
  , zip
  , zip3
  , zipWith
  , lines
  , words
  ) where

import Prelude hiding (not, id, (.), const, flip, fmap, (<$>), fst, snd, curry, uncurry, map, reverse, zip, zip3, unzip, zipWith, lines, words)

import Data.Bijection.Type
import Data.Bijection.Bool
import Data.Bijection.Function
import Data.Bijection.Functor
import Data.Bijection.Tuple
import Data.Bijection.List

-- |Convert between an 'Int' and an 'Enum' with 'P.toEnum' and 'P.fromEnum'.
enum :: Enum a => Int <-> a
enum = toEnum :<->: fromEnum
