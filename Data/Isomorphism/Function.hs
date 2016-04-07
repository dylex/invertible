-- |
-- Versions of functions from "Data.Function" as isomorphisms.
{-# LANGUAGE Safe #-}
module Data.Isomorphism.Function
  ( id
  , (.)
  , consts
  , const
  , flip
  ) where

import Prelude hiding (id, (.), const, flip)
import qualified Control.Category as C
import qualified Data.Function as F

import Data.Isomorphism.Type

-- |Identity isomorphism.
id :: a <-> a
id = C.id

-- |Isomorphism composition
(.) :: (b <-> c) -> (a <-> b) -> a <-> c
(.) = (C..)
infixr 9 .

-- |Bidirectional constant function (not a true isomorphism).
consts :: a -> b -> a <-> b
consts a b = F.const b :<->: F.const a

-- |Convert between '()' and a constant (not a true isomorphism).
const :: a -> () <-> a
const = consts ()

-- |'F.flip' the order of the first two arguments of a function.
flip :: (a -> b -> c) <-> (b -> a -> c)
flip = F.flip :<->: F.flip
