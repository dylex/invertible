-- |
-- Use isomorphisms with 'Invariant' functors from "Data.Functor.Invariant".
module Data.Isomorphism.Invariant
  ( invmap
  , isomap
  ) where

import Data.Isomorphism.Type
import Data.Isomorphism.Prelude (invert)
import qualified Data.Functor.Invariant as I

-- |Apply an isomorphism over an 'Invariant' using 'I.invmap'.
invmap :: I.Invariant f => a <-> b -> f a -> f b
invmap (f :<->: g) = I.invmap f g

-- |Lift an isomorphism over an 'Invariant' functor.
-- There is probably no need for this function.
isomap :: I.Invariant f => a <-> b -> f a <-> f b
isomap f = invmap f :<->: invmap (invert f)
