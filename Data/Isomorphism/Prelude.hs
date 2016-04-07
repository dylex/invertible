-- |
-- The \"Prelude\" for isomorphism, which re-exports various isomorphisms similar to functions from "Prelude".
-- Most \"un\"-functions are left out for obvious reasons.
{-# LANGUAGE Safe #-}
module Data.Isomorphism.Prelude
  ( module Data.Isomorphism.Type
  , module Data.Isomorphism.Internal

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

import Data.Isomorphism.Type
import Data.Isomorphism.Internal
import Data.Isomorphism.Bool
import Data.Isomorphism.Function
import Data.Isomorphism.Functor
import Data.Isomorphism.Tuple
import Data.Isomorphism.List

-- |Convert between an 'Int' and an 'Enum' with 'P.toEnum' and 'P.fromEnum'.
enum :: Enum a => Int <-> a
enum = toEnum :<->: fromEnum
