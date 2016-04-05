-- |
-- Versions of functions from "Data.Tuple" as isomorphisms.
module Data.Isomorphism.Tuple
  ( fst
  , snd
  , curry
  , swap
  ) where

import Prelude hiding (fst, snd, curry, uncurry)
import qualified Data.Tuple as T

import Data.Isomorphism.Type
import Data.Isomorphism.TH

-- |Extract the 'T.fst' component of a pair.
fst :: (a, ()) <-> a
fst = [isoCase|(a, ()) <-> a|]

-- |Extract the 'T.snd' component of a pair.
snd :: ((), a) <-> a
snd = [isoCase|((), a) <-> a|]

-- |Convert between an uncurried function and a 'T.curry'ed function.
curry :: ((a, b) -> c) <-> (a -> b -> c)
curry = T.curry :<->: T.uncurry

-- |'T.swap' the components of a pair.
swap :: (a, b) <-> (b, a)
swap = [isoCase|(a, b) <-> (b, a)|]
