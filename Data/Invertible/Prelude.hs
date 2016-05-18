-- |
-- The bidirectional \"Prelude\", which re-exports various bijections similar to functions from "Prelude".
-- Most \"un\"-functions are left out for obvious reasons.
{-# LANGUAGE Safe, TypeOperators #-}
module Data.Invertible.Prelude
  ( (<->)
  , type (<->)

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
  , bifmap
  , Functor(..)
  , (<$>)
  , map
  , reverse
  , zip
  , zip3
  , zipWith
  , lines
  , words
  ) where

import Prelude hiding (not, id, (.), const, flip, Functor(..), (<$>), fst, snd, curry, uncurry, map, reverse, zip, zip3, unzip, zipWith, lines, words)

import Control.Invertible.BiArrow
import Control.Invertible.Functor
import Data.Invertible.Bijection
import Data.Invertible.Bool
import Data.Invertible.Function
import Data.Invertible.Functor
import Data.Invertible.Tuple
import Data.Invertible.List

-- |Convert between an 'Int' and an 'Enum' with 'P.toEnum' and 'P.fromEnum'.
enum :: Enum a => Int <-> a
enum = toEnum :<->: fromEnum
