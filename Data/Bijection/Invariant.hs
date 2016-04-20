-- |
-- Use bijections on 'Invariant' functors from "Data.Functor.Invariant".
module Data.Bijection.Invariant
  ( invmap
  ) where

import Data.Bijection.Type
import qualified Data.Functor.Invariant as I

-- |Apply an isomorphism over an 'Invariant' using 'I.invmap'.
invmap :: I.Invariant f => a <-> b -> f a -> f b
invmap (f :<->: g) = I.invmap f g
