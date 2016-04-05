-- |
-- The \"Prelude\" for isomorphism, which re-exports various isomorphisms similar to functions from "Prelude".
-- Most \"un\"-functions are left out for obvious reasons.
module Data.Isomorphism.Prelude
  ( module Data.Isomorphism.Type
  , (C.>>>)
  , (C.<<<)
  , module Data.Isomorphism.Internal

  , enum

  , const
  , flip
  , not
  , fst
  , snd
  , curry
  , cons
  , uncons
  , fmap
  , reverse
  , zip
  , lines
  , words
  ) where

import Prelude hiding (not, const, flip, fmap, fst, snd, curry, uncurry, reverse, zip, unzip, lines, words)
import qualified Control.Category as C

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
