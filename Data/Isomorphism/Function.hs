-- |
-- Versions of functions from "Data.Function" as isomorphisms.
{-# LANGUAGE Safe #-}
module Data.Isomorphism.Function
  ( consts
  , const
  , flip
  ) where

import Prelude hiding (const, flip)
import qualified Data.Function as F

import Data.Isomorphism.Type

-- |Bidirectional constant function (not a true isomorphism).
consts :: a -> b -> a <-> b
consts a b = F.const b :<->: F.const a

-- |Convert between '()' and a constant (not a true isomorphism).
const :: a -> () <-> a
const = consts ()

-- |'F.flip' the order of the first two arguments of a function.
flip :: (a -> b -> c) <-> (b -> a -> c)
flip = F.flip :<->: F.flip
