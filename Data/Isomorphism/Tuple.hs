-- |
-- Versions of functions from "Data.Tuple" as isomorphisms.
{-# LANGUAGE Safe #-}
module Data.Isomorphism.Tuple
  ( fst
  , snd
  , curry
  , swap
  , flatten1_2
  , flatten1_3
  , flatten1_4
  , flatten2_1
  , flatten2_2
  , flatten3_1
  , flatten4_1
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

flatten2_1 :: ((a, b), c) <-> (a, b, c)
flatten2_1 = [isoCase|((a, b), c) <-> (a, b, c)|]

flatten1_2 :: (a, (b, c)) <-> (a, b, c)
flatten1_2 = [isoCase|(a, (b, c)) <-> (a, b, c)|]

flatten3_1 :: ((a, b, c), d) <-> (a, b, c, d)
flatten3_1 = [isoCase|((a, b, c), d) <-> (a, b, c, d)|]

flatten1_3 :: (a, (b, c, d)) <-> (a, b, c, d)
flatten1_3 = [isoCase|(a, (b, c, d)) <-> (a, b, c, d)|]

flatten2_2 :: ((a, b), (c, d)) <-> (a, b, c, d)
flatten2_2 = [isoCase|((a, b), (c, d)) <-> (a, b, c, d)|]

flatten1_4 :: (a, (b, c, d, e)) <-> (a, b, c, d, e)
flatten1_4 = [isoCase|(a, (b, c, d, e)) <-> (a, b, c, d, e)|]

flatten4_1 :: ((a, b, c, d), e) <-> (a, b, c, d, e)
flatten4_1 = [isoCase|((a, b, c, d), e) <-> (a, b, c, d, e)|]
